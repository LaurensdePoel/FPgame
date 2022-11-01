{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module defines the Updates of entities in the game
module Updates where

import Animateable
import Assets
import Behaviour
import Collidable
import Config
import Damageable
import Data.Maybe
import qualified Data.Set as S
import Foreign (toBool)
import Graphics.Gloss.Interface.IO.Interact
import Init
import Input
import Menu
import Model
import Timeable
import Updateable

-- ! FILENAME HANDLER !!
-- TODO Naming refactor
-- TODO REWRITE FUNCTIONS

-- Handles levels and waves
levelHandler :: GameState -> GameState
levelHandler gs@GameState {level = _level, enemies = _enemies, players = _players}
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
    ifAllWavesCleared = null (waves _level)
    ifAllPlayersDied :: Bool
    ifAllPlayersDied = null _players
    ifWaveTimerExpired :: Bool
    ifWaveTimerExpired = readyToExecute _level

-- Handles all timers of entities
timeHandler :: GameState -> GameState
timeHandler gs@GameState {players = _players, enemies = _enemies, level = _level, projectiles = _projectiles, powerUps = _powerUps, particles = _particles} =
  gs {players = updatedPlayers, enemies = updatedEnemies, level = updatedLevel, projectiles = updatedProjectiles, powerUps = updatedPowerUps, particles = updatedParticles}
  where
    updatedPlayers = map (\player -> updateTime player {airplanePowerUps = map updateTime (airplanePowerUps player)}) _players
    updatedEnemies = map updateTime _enemies
    updatedProjectiles = _projectiles ++ concatMap shoot (filter readyToExecute (updatedPlayers ++ updatedEnemies))
    updatedPowerUps = map updateTime _powerUps
    updatedParticles = map (onChange nextSprite) _particles
    updatedLevel = updateTime _level

-- Handles all movement of entities
movementHandler :: GameState -> GameState
movementHandler gs@GameState {players = _players, enemies = _enemies, projectiles = _projectiles} =
  gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles}
  where
    updatedPlayers = map (move . updatePlayerVelocity (pressedKeys gs)) _players
    updatedEnemies = map move _enemies
    updatedProjectiles = map move _projectiles

-- Handles all particles (creating new particles on certain events)
particleHandler :: GameState -> GameState
particleHandler gs@GameState {particles = _particles, particleMap = _particleMap} = gs {particles = _particles ++ newParticles ++ newParticles2}
  where
    newParticles = mapMaybe (\airplane -> if isNothing $ destroy airplane then let newParticle = getParticle "explosion2" _particleMap in Just newParticle {particlePosition = centerPosition (airplanePos airplane) (airplaneSize airplane)} else Nothing) (players gs ++ enemies gs)
    newParticles2 = mapMaybe (\projectile -> if isNothing $ destroy projectile then let newParticle = getParticle "explosion" _particleMap in Just newParticle {particlePosition = centerPosition (projectilePos projectile) (projectileSize projectile)} else Nothing) $ projectiles gs
    centerPosition :: Position -> Size -> Position
    centerPosition (x, y)  (xx, yy) = (x + (xx * 0.5), y - (yy * 0.5))

-- Handles collision (events on collision) between all entities which are collidable
collisionHandler :: GameState -> GameState
collisionHandler gs@GameState {players = _players, enemies = _enemies, projectiles = _projectiles, powerUps = _powerUps} =
  gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles3, powerUps = updatedPowerUps}
  where
    -- Handle collision between airplanes
    (players', enemies') = applyOnCollisions applyDamage3 _players _enemies

    applyDamage3 :: Airplane -> Airplane -> (Airplane, Airplane)
    applyDamage3 player enemy = (damage (airplaneHealth player) player, damage (airplaneHealth enemy) enemy)

    -- Handle collision between projectiles
    (updatedProjectiles, _) = applyOnCollisions applyDamage _projectiles _projectiles

    applyDamage :: Projectile -> Projectile -> (Projectile, Projectile)
    applyDamage a b = (damage (projectileDamage b) a, damage (projectileDamage a) b)

    -- Handle collision between projectiles and enemies
    (updatedProjectiles2, updatedEnemies) = applyOnCollisions applyDamage2 updatedProjectiles enemies'

    -- Handle collision between projectiles and players
    (updatedProjectiles3, players'') = applyOnCollisions applyDamage2 updatedProjectiles2 players'

    applyDamage2 :: Projectile -> Airplane -> (Projectile, Airplane)
    applyDamage2 a b = (damage (projectileHealth a) a, damage (projectileDamage a) b)

    -- Handle collision between players and powerUps
    (updatedPlayers, updatedPowerUps) = applyOnCollisions pickUp players'' _powerUps

    pickUp :: Airplane -> PowerUp -> (Airplane, PowerUp)
    pickUp a b = (powerUpEffect True a b, b {timeUntilDespawn = 0.0})

-- Removes all entities which satisfy their condition to be destroyed
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

-- ! Airplane.hs

updatePlayerVelocity :: S.Set Key -> Airplane -> Airplane
updatePlayerVelocity activeKeys airplane =
  foldr f e activeKeys
  where
    f = addVelocityBasedOnKey
    e = airplane

-- If key affects velocity of the player update the current velocity
addVelocityBasedOnKey :: Key -> Airplane -> Airplane
addVelocityBasedOnKey key airplane@Airplane {airplaneType = planeType, airplaneMaxVelocity = (minVel, maxVel)} =
  case planeType of
    Player1
      | key == Char 'w' -> airplane {airplaneVelocity = add (0, velocityStep)}
      | key == Char 'a' -> airplane {airplaneVelocity = add (- velocityStep, 0)}
      | key == Char 's' -> airplane {airplaneVelocity = add (0, - velocityStep)}
      | key == Char 'd' -> airplane {airplaneVelocity = add (velocityStep, -0)}
      | otherwise -> airplane
    Player2
      | key == SpecialKey KeyUp -> airplane {airplaneVelocity = add (0, velocityStep)}
      | key == SpecialKey KeyLeft -> airplane {airplaneVelocity = add (- velocityStep, 0)}
      | key == SpecialKey KeyDown -> airplane {airplaneVelocity = add (0, - velocityStep)}
      | key == SpecialKey KeyRight -> airplane {airplaneVelocity = add (velocityStep, -0)}
      | otherwise -> airplane
    _ -> airplane
  where
    add :: Velocity -> Velocity
    add vel = checkMinMax (airplaneVelocity airplane + vel)
    checkMinMax :: Velocity -> Velocity
    checkMinMax orignalVel@(vX, vY)
      | vX < minVel = (minVel, vY)
      | vY < minVel = (vX, minVel)
      | vX > maxVel = (maxVel, vY)
      | vY > maxVel = (vX, maxVel)
      | otherwise = orignalVel
    -- TODO move values below to special HS file those values are base parameters
    velocityStep = 0.6

shoot :: Airplane -> [Projectile]
shoot Airplane {airplanePos = (x, y), fireRate = r, airplaneGun = (AirplaneGun projectile)} = case r of
  Single _ -> [projectile {projectilePos = (x + gunOffset, y - gunOffset)}]
  Burst _ -> [projectile {projectilePos = (x - px - 5 + gunOffset, y - gunOffset)}, projectile {projectilePos = (x + gunOffset, y - gunOffset)}, projectile {projectilePos = (x - px - px - 10 + gunOffset, y - gunOffset)}] -- TODO: update: - or + is actually depended on if its a player or enemy
    where
       (px, _) = projectileSize projectile

-- Applies or removes powerUp effect from the airplane
-- TODO REFACTOR NAMES
powerUpEffect :: Bool -> Airplane -> PowerUp -> Airplane
powerUpEffect
  applyEffect
  player@Airplane {fireRate = fireRate', airplaneHealth = health, airplanePowerUps = pus}
  pu@PowerUp {powerUpType = puType} =
    case puType of
      PowerPack x -> case fireRate' of
        Single x' -> player {fireRate = Single $ updateBuffer x' x, airplanePowerUps = _pus}
        Burst x' -> player {fireRate = Burst $ updateBuffer x' x, airplanePowerUps = _pus}
      HealthPack x -> player {airplaneHealth = health + x}
    where
      _pus = pu {powerUpState = PickedUp} : pus
      updateBuffer :: Time -> Float -> Time
      updateBuffer value multiplier
        | applyEffect = value * multiplier
        | otherwise = value * (1 / multiplier)

-- ! Level.hs
nextWave :: GameState -> GameState
nextWave gs@GameState {level = _level, enemies = _enemies}
  | ifAllWavesCleared = gs -- do nothing if all waves are cleared --TODO better if we can disable timer
  | otherwise = gs {enemies = _enemies ++ spawnNextWave, level = _level {waves = removeWaveAfterSpawn}}
  where
    ifAllWavesCleared :: Bool
    ifAllWavesCleared = null (waves _level)

    spawnNextWave :: [Enemy]
    spawnNextWave = enemiesInWave $ head (waves _level)

    removeWaveAfterSpawn :: [Wave]
    removeWaveAfterSpawn = tail $ waves _level

-- !  Controller
updateGameState :: GameState -> GameState
updateGameState = debugButtons . checkPause . levelHandler . garbageCollector . particleHandler . collisionHandler . timeHandler . movementHandler . enemyBehaviourHandler -- . destroyObjects . updateParticles . updateAirplanes . updateProjectiles . updatePowerUps

checkPause :: GameState -> GameState
checkPause gs@GameState {pressedKeys = _pressedKeys} = singleKeyPress (SpecialKey KeyEsc) gs pauseMenu

debugButtons :: GameState -> GameState
debugButtons = debugSpawnButton . debugKillEnemyButton
  where
    debugSpawnButton :: GameState -> GameState
    debugSpawnButton gs@GameState {pressedKeys = _pressedKeys} = singleKeyPress (Char 't') gs nextWave

    debugKillEnemyButton :: GameState -> GameState
    debugKillEnemyButton gs@GameState {pressedKeys = _pressedKeys} = singleKeyPress (Char 'k') gs killTopEnemy
      where
        killTopEnemy gs@GameState {enemies = _enemies}
          | null _enemies = gs
          | otherwise = gs {enemies = tail _enemies}

updateMenu :: GameState -> GameState
updateMenu = checkMenuInput
