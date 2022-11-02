{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module defines the Updates of entities in the game
module Handler where

import Airplane
import Animateable
import Assets
import Collidable
import Damageable
import Data.Maybe
import Init
import Input
import Level
import Model
import Player
import Timeable (Timeable (applyOnExecute, readyToExecute, updateTime))
import Updateable

-- TODO Naming refactor
-- TODO REWRITE FUNCTIONS

-- | Handles levels and waves -- TODO Rewrite so more functonality is inside Level.hs
levelHandler :: GameState -> GameState
levelHandler gs@GameState {levels = _levels, enemies = _enemies, players = _players}
  -- Enter Defeat menu
  | ifAllPlayersDied = gs {status = InMenu, menu = initDefeatMenu, pressedKeys = emptyKeys}
  -- Enter Victory menu
  | ifCurrentWaveKilled && ifAllWavesCleared = gs {status = InMenu, menu = initVictoryMenu, pressedKeys = emptyKeys}
  -- Next Wave
  | ifCurrentWaveKilled || ifWaveTimerExpired = nextWave gs
  -- Do Nothing
  | otherwise = gs
  where
    ifCurrentWaveKilled :: Bool
    ifCurrentWaveKilled = null _enemies
    ifAllWavesCleared :: Bool
    ifAllWavesCleared = null (waves _levels)
    ifAllPlayersDied :: Bool
    ifAllPlayersDied = null _players
    ifWaveTimerExpired :: Bool
    ifWaveTimerExpired = readyToExecute _levels

-- | Handles all timers of entities
timeHandler :: GameState -> GameState
timeHandler gs@GameState {players = _players, enemies = _enemies, levels = _levels, projectiles = _projectiles, powerUps = _powerUps, particles = _particles} =
  gs {players = updatedPlayers, enemies = updatedEnemies, levels = updatedLevel, projectiles = updatedProjectiles, powerUps = updatedPowerUps, particles = updatedParticles}
  where
    updatedPlayers = map (\player -> updateTime player {airplanePowerUps = map updateTime (airplanePowerUps player)}) _players
    updatedEnemies = map updateTime _enemies
    updatedProjectiles = _projectiles ++ concatMap shoot (filter readyToExecute (updatedPlayers ++ updatedEnemies))
    updatedPowerUps = map updateTime _powerUps
    updatedParticles = map (applyOnExecute nextSprite) _particles
    updatedLevel = updateTime _levels

-- | Handles all movement of entities
movementHandler :: GameState -> GameState
movementHandler gs@GameState {players = _players, enemies = _enemies, projectiles = _projectiles} =
  gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles}
  where
    updatedPlayers = map (move . updatePlayerVelocity (pressedKeys gs)) _players
    updatedEnemies = map move _enemies
    updatedProjectiles = map move _projectiles

-- | Handles all particles (creating new particles on certain events)
particleHandler :: GameState -> GameState
particleHandler gs@GameState {particles = _particles, particleMap = _particleMap} =
  gs {particles = _particles ++ newParticles ++ newParticles2}
  where
    newParticles = mapMaybe (\airplane -> if isNothing $ destroy airplane then let newParticle = getParticle "explosion2" _particleMap in Just newParticle {particlePosition = centerPosition (airplanePos airplane) (airplaneSize airplane)} else Nothing) (players gs ++ enemies gs)
    newParticles2 = mapMaybe (\projectile -> if isNothing $ destroy projectile then let newParticle = getParticle "explosion" _particleMap in Just newParticle {particlePosition = centerPosition (projectilePos projectile) (projectileSize projectile)} else Nothing) $ projectiles gs
    centerPosition :: Position -> Size -> Position
    centerPosition (x, y) (xx, yy) = (x + (xx * 0.5), y - (yy * 0.5))

-- | Handles collision (events on collision) between all entities which are collidable
collisionHandler :: GameState -> GameState
collisionHandler gs@GameState {players = _players, enemies = _enemies, projectiles = _projectiles, powerUps = _powerUps} =
  gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles3, powerUps = updatedPowerUps}
  where
    -- Handle collision between airplanes
    (players', enemies') = applyOnCollisions applyDamage3 _players _enemies

    applyDamage3 :: Airplane -> Airplane -> (Airplane, Airplane)
    applyDamage3 player enemy = (takeDamage (airplaneHealth player) player, takeDamage (airplaneHealth enemy) enemy)

    -- Handle collision between projectiles
    (updatedProjectiles, _) = applyOnCollisions applyDamage _projectiles _projectiles

    applyDamage :: Projectile -> Projectile -> (Projectile, Projectile)
    applyDamage a b = (takeDamage (projectileDamage b) a, takeDamage (projectileDamage a) b)

    -- Handle collision between projectiles and enemies
    (updatedProjectiles2, updatedEnemies) = applyOnCollisions applyDamage2 updatedProjectiles enemies'

    -- Handle collision between projectiles and players
    (updatedProjectiles3, players'') = applyOnCollisions applyDamage2 updatedProjectiles2 players'

    applyDamage2 :: Projectile -> Airplane -> (Projectile, Airplane)
    applyDamage2 a b = (takeDamage (projectileHealth a) a, takeDamage (projectileDamage a) b)

    -- Handle collision between players and powerUps
    (updatedPlayers, updatedPowerUps) = applyOnCollisions pickUp players'' _powerUps

    pickUp :: Airplane -> PowerUp -> (Airplane, PowerUp)
    pickUp a b = (powerUpEffect True a b, b {timeUntilDespawn = 0.0})

-- | Removes all entities which satisfy their condition to be destroyed
garbageCollector :: GameState -> GameState
garbageCollector gs@GameState {players = _players, enemies = _enemies, projectiles = _projectiles, powerUps = _powerUps, particles = _particles} =
  gs {players = updatedPlayers, enemies = destroyFromList _enemies, projectiles = destroyFromList _projectiles, powerUps = destroyFromList _powerUps, particles = destroyFromList _particles}
  where
    -- Remove destroyed players or/and finished powerUps of players
    updatedPlayers = mapMaybe (updatePowerUps . destroy) $ destroyFromList _players
      where
        updatePowerUps :: Maybe Airplane -> Maybe Airplane
        updatePowerUps player =
          case player of
            Just _player -> let updatedPlayer = foldr (\powerUp r -> if readyToExecute powerUp then powerUpEffect False r powerUp else r) _player (airplanePowerUps _player) in Just updatedPlayer {airplanePowerUps = mapMaybe destroy (airplanePowerUps _player)}
            Nothing -> Nothing