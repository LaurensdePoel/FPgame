-- | This module defines the Updates of entities in the game
module Updates where

import Animateable
import Behaviour
import Collidable
import Config
import Damageable
import Data.Maybe
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact
import Input
import Menu
import Model
import Timeable
import Updateable

-- TODO maybe add this to Input.hs so other functions can also use keypress thats is beeing hold down
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
      | key == Char 'a' -> airplane {airplaneVelocity = add (-velocityStep, 0)}
      | key == Char 's' -> airplane {airplaneVelocity = add (0, -velocityStep)}
      | key == Char 'd' -> airplane {airplaneVelocity = add (velocityStep, -0)}
      | otherwise -> airplane
    Player2
      | key == SpecialKey KeyUp -> airplane {airplaneVelocity = add (0, velocityStep)}
      | key == SpecialKey KeyLeft -> airplane {airplaneVelocity = add (-velocityStep, 0)}
      | key == SpecialKey KeyDown -> airplane {airplaneVelocity = add (0, -velocityStep)}
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
      (Size (px, _)) = projectileSize projectile

-- -- TODO: handle airplane collides screenbox
-- updateAirplanes :: GameState -> GameState
-- -- updateAirplanes gs@GameState {players = players, enemies = enemies, projectiles = projectiles} = gs {players = players', enemies = enemies', projectiles = newProjectiles ++ projectiles}  -- maybe moves, update fire rate, maybe add projectile to projectile list, take damage if collides with enemy plane(also other object) , maybe destroy
-- updateAirplanes gs@Game {players = players, enemies = enemies, projectiles = projectiles, pressedKeys = pressedKeys} = gs {players = players', enemies = enemies', projectiles = newProjectiles ++ projectiles} -- maybe moves, update fire rate, maybe add projectile to projectile list, take damage if collides with enemy plane(also other object) , maybe destroy
--   where
--     updatedPlayers :: [Airplane]
--     updatedPlayers = map (updateTime . move . updatePlayerVelocity pressedKeys) players

--     updatedEnemies :: [Airplane]
--     updatedEnemies = map (updateTime . move) enemies

--     newProjectiles :: [Projectile]
--     newProjectiles = concatMap shoot $ filter readyToExecute (updatedPlayers ++ updatedEnemies)

--     (players', enemies') = applyOnCollisions applyDamage updatedPlayers updatedEnemies
--     applyDamage :: Airplane -> Airplane -> (Airplane, Airplane)
--     applyDamage player enemy = (damage (airplaneHealth player) player, damage (airplaneHealth enemy) enemy)

-- -- ToDO: handle all updates ||||| for now it only checks if projectile is still on screen
-- updateProjectiles :: GameState -> GameState
-- updateProjectiles gs@Game {players = players, enemies = enemies, projectiles = projectiles} = gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles3}
--   where
--     projectiles' = map (move) projectiles
--     (updatedProjectiles, _) = applyOnCollisions applyDamage projectiles' projectiles'
--     (updatedProjectiles2, updatedEnemies) = applyOnCollisions applyDamage2 updatedProjectiles enemies
--     (updatedProjectiles3, updatedPlayers) = applyOnCollisions applyDamage2 updatedProjectiles2 players
--     applyDamage :: Projectile -> Projectile -> (Projectile, Projectile)
--     applyDamage a b = (damage (projectileDamage b) a, damage (projectileDamage a) b)

--     applyDamage2 :: Projectile -> Airplane -> (Projectile, Airplane)
--     applyDamage2 a b = (damage (projectileHealth a) a, damage (projectileDamage a) b)

-- destroys all objects with zero health
-- destroyObjects :: GameState -> GameState
-- destroyObjects gs@Game {players = players, enemies = enemies, projectiles = projectiles, particles = _particles} = gs {players = destroyFromList players, enemies = destroyFromList enemies, projectiles = destroyFromList projectiles, particles = destroyFromList _particles}

checkPause :: GameState -> GameState
checkPause gs@Game {pressedKeys = _pressedKeys} = singleKeyPress (SpecialKey KeyEsc) gs pauseMenu

-- updatePowerUps :: GameState -> GameState
-- updatePowerUps gs@Game {players = players', powerUps = powerUps'} = gs {players = updatedPlayers4, powerUps = updatedPowerUps2}
--   where
--     updatedPowerUps = mapMaybe (destroy . updateTime) powerUps'
--     updatedPowerUps2 = map (\powerUp -> foldr (\player r -> if player `collides` powerUp then r {timeUntilDespawn = 0.0} else r) powerUp players') updatedPowerUps
--     updatedPlayers = map (\player -> foldr (\powerUp r -> if player `collides` powerUp then applyPowerUp r powerUp else r) player updatedPowerUps) players'
--     updatedPlayers2 = map (\player -> player {airplanePowerUps = map updateTime (airplanePowerUps player)}) updatedPlayers

--     updatedPlayers3 = map (\player -> foldr (\powerUp r -> if readyToExecute powerUp then removePowerUpEffect r powerUp else r) player (airplanePowerUps player)) updatedPlayers2
--     updatedPlayers4 = map (\player -> player {airplanePowerUps = mapMaybe (destroy) (airplanePowerUps player)}) updatedPlayers3

--     applyPowerUp :: Airplane -> PowerUp -> Airplane
--     applyPowerUp
--       player@Airplane {fireRate = fireRate', airplaneHealth = health, airplanePowerUps = ups}
--       pu@PowerUp {powerUpType = puType} =
--         case puType of
--           PowerPack x -> case fireRate' of
--             Single x' -> player {fireRate = Single (x' * x), airplanePowerUps = _pu : ups}
--             Burst x' -> player {fireRate = Single (x' * x), airplanePowerUps = _pu : ups}
--           HealthPack x -> player {airplaneHealth = health + x}
--         where
--           _pu = pu {powerUpState = PickedUp}

--     removePowerUpEffect :: Airplane -> PowerUp -> Airplane
--     removePowerUpEffect
--       player@Airplane {fireRate = fireRate'}
--       PowerUp {powerUpType = puType} =
--         case puType of
--           PowerPack x -> case fireRate' of
--             Single x' -> player {fireRate = Single (x' * (1 / x))}
--             Burst x' -> player {fireRate = Burst (x' * (1 / x))}
--           HealthPack _ -> player

-- updateParticles :: GameState -> GameState
-- -- updateParticles gs@Game {particles = _particles, players = players, enemies = enemies, projectiles = projectiles, particleMap = _particleMap} = gs {particles = map (\particle -> let p = updateTime particle in if readyToExecute p then nextSprite p else p) (_particles ++ newParticles ++ newParticles2 ++ newParticles3)}
-- updateParticles gs@Game {particles = _particles, players = players, enemies = enemies, projectiles = projectiles, particleMap = _particleMap} = gs {particles = map onChange (_particles ++ newParticles ++ newParticles2 ++ newParticles3)}
--   where
--     newParticles = mapMaybe (\player -> if 0 >= airplaneHealth player then let newParticle = getParticle "explosion2" _particleMap in Just newParticle {particlePosition = airplanePos player} else Nothing) players
--     newParticles2 = mapMaybe (\enemy -> if 0 >= airplaneHealth enemy then let newParticle = getParticle "explosion2" _particleMap in Just newParticle {particlePosition = airplanePos enemy} else Nothing) enemies
--     newParticles3 = mapMaybe (\projectile -> if 0 >= projectileHealth projectile then let newParticle = getParticle "explosion" _particleMap in Just newParticle {particlePosition = projectilePos projectile} else Nothing) projectiles

updateGameState :: GameState -> GameState
updateGameState = checkPause . garbageCollector . particleHandler . collisionHandler . timeHandler . movementHandler . enemyBehaviourHandler -- . destroyObjects . updateParticles . updateAirplanes . updateProjectiles . updatePowerUps

-- pauseMenu :: GameState -> GameState
-- pauseMenu gs@Game {status = _status} = gs {status = toggleStatus}
--   where
--     toggleStatus :: Status
--     toggleStatus = case _status of
--       InMenu -> InGame
--       InGame -> InMenu

-- Handles all timers of entities
timeHandler :: GameState -> GameState
timeHandler gs@Game {players = _players, enemies = _enemies, projectiles = _projectiles, powerUps = _powerUps, particles = _particles} =
  gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles, powerUps = updatedPowerUps, particles = updatedParticles}
  where
    updatedPlayers = map (\player -> updateTime player {airplanePowerUps = map updateTime (airplanePowerUps player)}) _players
    updatedEnemies = map updateTime _enemies
    updatedProjectiles = _projectiles ++ (concatMap shoot $ filter readyToExecute (updatedPlayers ++ updatedEnemies))
    updatedPowerUps = map updateTime _powerUps
    updatedParticles = map (onChange nextSprite) _particles

-- Handles all movement of entities
movementHandler :: GameState -> GameState
movementHandler gs@Game {players = _players, enemies = _enemies, projectiles = _projectiles} =
  gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles}
  where
    updatedPlayers = map (move . (updatePlayerVelocity $ pressedKeys gs)) _players
    updatedEnemies = map move _enemies
    updatedProjectiles = map move _projectiles

-- Handles all particles (creating new particles on certain events)
particleHandler :: GameState -> GameState
particleHandler gs@Game {particles = _particles, particleMap = _particleMap} = gs {particles = _particles ++ newParticles ++ newParticles2}
  where
    newParticles = mapMaybe (\airplane -> if isNothing $ destroy airplane then let newParticle = getParticle "explosion2" _particleMap in Just newParticle {particlePosition = centerPosition (airplanePos airplane) (airplaneSize airplane)} else Nothing) (players gs ++ enemies gs)
    newParticles2 = mapMaybe (\projectile -> if isNothing $ destroy projectile then let newParticle = getParticle "explosion" _particleMap in Just newParticle {particlePosition = centerPosition (projectilePos projectile) (projectileSize projectile)} else Nothing) $ projectiles gs
    centerPosition :: Position -> Size -> Position
    centerPosition (x, y) (Size (xx, yy)) = (x + (xx * 0.5), y - (yy * 0.5))

-- Handles collision (events on collision) between all entities which are collidable
collisionHandler :: GameState -> GameState
collisionHandler gs@Game {players = _players, enemies = _enemies, projectiles = _projectiles, powerUps = _powerUps} =
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
garbageCollector gs@Game {players = _players, enemies = _enemies, projectiles = _projectiles, powerUps = _powerUps, particles = _particles} =
  gs {players = updatedPlayers, enemies = destroyFromList _enemies, projectiles = destroyFromList _projectiles, powerUps = destroyFromList _powerUps, particles = destroyFromList _particles}
  where
    -- Remove destroyed players or/and finished powerUps of players
    updatedPlayers = mapMaybe (updatePowerUps . destroy) $ destroyFromList _players
      where
        updatePowerUps :: Maybe Airplane -> Maybe Airplane
        updatePowerUps player =
          case player of
            Just _player -> let updatedPlayer = (foldr (\powerUp r -> if readyToExecute powerUp then powerUpEffect False r powerUp else r) _player (airplanePowerUps _player)) in Just updatedPlayer {airplanePowerUps = mapMaybe (destroy) (airplanePowerUps _player)}
            Nothing -> Nothing

-- Applies or removes powerUp effect from the airplane
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
        | applyEffect = (value * multiplier)
        | otherwise = (value * (1 / multiplier))

updateMenu :: GameState -> GameState
updateMenu = checkPause . checkMenuInput
