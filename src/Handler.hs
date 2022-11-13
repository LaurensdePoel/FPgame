{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module defines the Updates of entities in the game
module Handler where

import Airplane (shoot)
import Animateable (Animateable (nextSprite))
import Assets (getParticle)
import Collidable (Collidable (applyOnCollisions))
import Config as C
import Damageable (Damageable (damageBoth))
import Data.Maybe (isNothing, mapMaybe)
import Init (initDefeatMenu, initVictoryMenu)
import Input (emptyKeys)
import Level (nextWave)
import Model
import Player (powerUpEffect, updatePlayerVelocity)
import Timeable (Timeable (applyOnExecute, readyToExecute, updateTime))
import Updateable
  ( Updateable (destroy, destroyFromList, getCenterPosition, move),
  )

-- | Handles levels and waves -- TODO Rewrite so more functonality is inside Level.hs
levelHandler :: GameState -> GameState
levelHandler gs@GameState {currentLevel = _currentLevel, assetMap = _assetsList, enemies = _enemies, players = _players}
  -- Enter Defeat menu
  | ifAllPlayersDied = gs {status = InMenu, menu = initDefeatMenu _assetsList, pressedKeys = emptyKeys}
  -- Enter Victory menu
  | ifCurrentWaveKilled && ifAllWavesCleared = gs {status = InMenu, menu = initVictoryMenu _assetsList, pressedKeys = emptyKeys}
  -- Next Wave
  | ifCurrentWaveKilled || ifWaveTimerExpired = nextWave gs
  -- Do Nothing
  | otherwise = gs
  where
    ifCurrentWaveKilled :: Bool
    ifCurrentWaveKilled = null _enemies
    ifAllWavesCleared :: Bool
    ifAllWavesCleared = null (waves _currentLevel)
    ifAllPlayersDied :: Bool
    ifAllPlayersDied = null _players
    ifWaveTimerExpired :: Bool
    ifWaveTimerExpired = readyToExecute _currentLevel

-- | Handles all timers of entities
timeHandler :: GameState -> GameState
timeHandler gs@GameState {players = _players, enemies = _enemies, currentLevel = _currentLevel, projectiles = _projectiles, powerUps = _powerUps, particles = _particles} =
  gs {players = updatedPlayers, enemies = updatedEnemies, currentLevel = updatedLevel1, projectiles = updatedProjectiles, powerUps = updatedPowerUps, particles = updatedParticles}
  where
    updatedPlayers = map (\player -> updateTime player {airplanePowerUps = map updateTime (airplanePowerUps player)}) _players
    updatedEnemies = map updateTime _enemies
    updatedProjectiles = _projectiles ++ concatMap shoot (filter readyToExecute (updatedPlayers ++ updatedEnemies))
    updatedPowerUps = map updateTime _powerUps
    updatedParticles = map (applyOnExecute nextSprite) _particles
    updatedLevel = updateTime _currentLevel
    updatedLevel1 = updatedLevel {levelBackground = nextSprite $ levelBackground updatedLevel}

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
particleHandler gs@GameState {players = _players, enemies = _enemies, projectiles = _projectiles, particles = _particles, particleMap = _particleMap} =
  gs {particles = _particles ++ newParticles}
  where
    newParticles = getParticles _particleMap "explosion2" (_players ++ _enemies) ++ getParticles _particleMap "explosion" _projectiles

    -- \| Create all new particles
    getParticles :: Updateable a => Particles -> String -> [a] -> [Particle]
    getParticles particleMap' key = mapMaybe (\a -> if isNothing $ destroy a then Just $ createParticle a else Nothing)
      where
        newParticle = getParticle key particleMap'
        createParticle :: Updateable a => a -> Particle
        createParticle a' = newParticle {particlePos = getCenterPosition a'}

-- | Handles collision (events on collision) between all entities which are collidable
collisionHandler :: GameState -> GameState
collisionHandler gs@GameState {players = _players, enemies = _enemies, projectiles = _projectiles, powerUps = _powerUps} =
  gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles3, powerUps = updatedPowerUps}
  where
    -- Handle collision between airplanes
    (players', enemies') = applyOnCollisions damageBoth _players _enemies

    -- Handle collision between projectiles
    (updatedProjectiles, _) = applyOnCollisions damageBoth _projectiles _projectiles

    -- Handle collision between projectiles and enemies
    (updatedProjectiles2, updatedEnemies) = applyOnCollisions damageBoth updatedProjectiles enemies'

    -- Handle collision between projectiles and players
    (updatedProjectiles3, players'') = applyOnCollisions damageBoth updatedProjectiles2 players'

    -- Handle collision between players and powerUps
    (updatedPlayers, updatedPowerUps) = applyOnCollisions pickUp players'' _powerUps

    pickUp :: Airplane -> PowerUp -> (Airplane, PowerUp)
    pickUp a b = (powerUpEffect True a b, b {timeUntilDespawn = C.resetTime})

-- | Removes all entities which satisfy their condition to be destroyed
garbageCollector :: GameState -> GameState
garbageCollector gs@GameState {players = _players, enemies = _enemies, projectiles = _projectiles, particles = _particles, powerUps = _powerUps} =
  gs {players = updatedPlayers, enemies = destroyFromList _enemies, projectiles = destroyFromList _projectiles, particles = destroyFromList updatedPowerUpParticles, powerUps = destroyFromList _powerUps}
  where
    -- Remove power up particle when the power up is destroyed
    updatedPowerUpParticles :: [Particle]
    updatedPowerUpParticles = filter (\particle -> let pos = particlePos particle in pos `notElem` positions) _particles
      where
        positions :: [Position]
        -- positions = mapMaybe (\powerUp -> if isNothing $ destroy powerUp then Just $ getClosestPosition powerUp _particles else Nothing) _powerUps
        positions = mapMaybe (\powerUp -> if isNothing $ destroy powerUp then Just $ powerUpPos powerUp + C.itemParticleOffset else Nothing) _powerUps

    -- Remove destroyed players or/and finished powerUps of players
    updatedPlayers = mapMaybe (updatePowerUps . destroy) $ destroyFromList _players
      where
        updatePowerUps :: Maybe Airplane -> Maybe Airplane
        updatePowerUps player =
          case player of
            Just player' -> Just (updatedPlayer player') {airplanePowerUps = mapMaybe destroy (airplanePowerUps player')}
            Nothing -> Nothing
          where
            updatedPlayer :: Airplane -> Airplane
            updatedPlayer player'' =
              foldr
                ( \powerUp r ->
                    if readyToExecute powerUp
                      then powerUpEffect False r powerUp
                      else r
                )
                player''
                (airplanePowerUps player'')
