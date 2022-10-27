-- | This module defines the Updates of entities in the game
module Updates where

import Collidable
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact
import Input
import Model
import Timeable
import Updateable

-- singleKeyPressStatus :: GameState -> GameState
-- singleKeyPressStatus gs@Game {status = _status, pressedKeys = _pressedKeys}
--   | S.member (SpecialKey KeyEsc) _pressedKeys = gs {status = toggleStatus, pressedKeys = S.delete (SpecialKey KeyEsc) _pressedKeys}
--   | otherwise = gs
--   where
--     toggleStatus :: Status
--     toggleStatus = case _status of
--       InMenu -> InGame
--       InGame -> InMenu

-- Updates velocity based on pressed keys. Foldr loops trough every key and add new velocity to current Airplane

-- TODO maybe add this to Input.hs so other functions can also use keypress thats is beeing hold down
updatePlayerVelocity :: S.Set Key -> Airplane -> Airplane
updatePlayerVelocity activeKeys airplane =
  foldr f e activeKeys
  where
    f = addVelocityBasedOnKey
    e = airplane

-- If key affects velocity of the player update the current velocity
addVelocityBasedOnKey :: Key -> Airplane -> Airplane
addVelocityBasedOnKey key airplane@Airplane {airplaneType = planeType} =
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
    minVel = -12.0
    maxVel = 12.0
    velocityStep = 0.6

shoot :: Airplane -> [Projectile]
shoot Airplane {airplanePos = (x, y), fireRate = r, airplaneProjectile = projectile} = case r of
  Single _ -> [projectile {projectilePos = (x + gunOffset, y - gunOffset)}]
  Burst _ -> [projectile {projectilePos = (x - px - 5 + gunOffset, y - gunOffset)}, projectile {projectilePos = (x + gunOffset, y - gunOffset)}, projectile {projectilePos = (x - px - px - 10 + gunOffset, y - gunOffset)}] -- TODO: update: - or + is actually depended on if its a player or enemy
    where
      (Size (px, _)) = projectileSize projectile

-- TODO: handle airplane collides screenbox
updateAirplanes :: GameState -> GameState
-- updateAirplanes gs@GameState {players = players, enemies = enemies, projectiles = projectiles} = gs {players = players', enemies = enemies', projectiles = newProjectiles ++ projectiles}  -- maybe moves, update fire rate, maybe add projectile to projectile list, take damage if collides with enemy plane(also other object) , maybe destroy
updateAirplanes gs@Game {players = players, enemies = enemies, projectiles = projectiles, pressedKeys = pressedKeys} = gs {players = players', enemies = enemies', projectiles = newProjectiles ++ projectiles} -- maybe moves, update fire rate, maybe add projectile to projectile list, take damage if collides with enemy plane(also other object) , maybe destroy
  where
    updatedPlayers :: [Airplane]
    updatedPlayers = map (updateTime . move . updatePlayerVelocity pressedKeys) players

    updatedEnemies :: [Airplane]
    updatedEnemies = map (updateTime . move) enemies

    newProjectiles :: [Projectile]
    newProjectiles = concatMap shoot $ filter readyToExecute (updatedPlayers ++ updatedEnemies)

    (players', enemies') = checkCollisions updatedPlayers updatedEnemies

    checkCollisions :: [Airplane] -> [Airplane] -> ([Airplane], [Airplane]) -- ToDo: makes this function better/ less ugly.
    checkCollisions [] pes = ([], pes)
    checkCollisions (pp : ps) pes = let (pp', pes') = checkCollisions ps pes in let (pList, eList) = pp `checkCollisions'` pes' in (pList : pp', eList)
      where
        checkCollisions' :: Airplane -> [Airplane] -> (Airplane, [Airplane])
        checkCollisions' p [] = (p, [])
        checkCollisions' pp (pe : pes) = case pp `collides` pe of
          True -> let (pp', pes') = checkCollisions' pp pes in ((damage (airplaneHealth pp') pp'), (damage (airplaneHealth pe) pe : pes'))
          False -> let (pp', pes') = checkCollisions' pp pes in (pp', (pe : pes'))

-- ToDO: handle all updates ||||| for now it only checks if projectile is still on screen
updateProjectiles :: GameState -> GameState
updateProjectiles gs@Game {players = players, enemies = enemies, projectiles = projectiles} = gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = updatedProjectiles3}
  where
    projectiles' = map (move) projectiles

    updatedEnemies :: [Airplane]
    updatedEnemies = map (\enemy -> foldr (\projectile r -> if projectile `collides` enemy then damage (projectileDamage projectile) r else r) enemy projectiles') enemies
    updatedPlayers :: [Airplane]
    updatedPlayers = map (\player -> foldr (\projectile r -> if projectile `collides` player then damage (projectileDamage projectile) r else r) player projectiles') players
    updatedProjectiles :: [Projectile]
    updatedProjectiles = map (\projectile -> foldr (\enemy r -> if projectile `collides` enemy then damage (projectileHealth projectile) r else r) projectile enemies) projectiles'
    updatedProjectiles2 = map (\projectile -> foldr (\player r -> if projectile `collides` player then damage (projectileHealth projectile) r else r) projectile players) updatedProjectiles
    updatedProjectiles3 = map (\projectile -> foldr (\projectile2 r -> if projectile `collides` projectile2 then damage (projectileHealth projectile) r else r) projectile updatedProjectiles2) updatedProjectiles2

-- destroys all objects with zero health
destroyObjects :: GameState -> GameState
destroyObjects gs@Game {players = players, enemies = enemies, projectiles = projectiles} = gs {players = destroyFromList players, enemies = destroyFromList enemies, projectiles = destroyFromList projectiles}

ckeckInput :: GameState -> GameState
ckeckInput gs@Game {pressedKeys = _pressedKeys} = singleKeyPress (SpecialKey KeyEsc) gs pauseMenu

updatePowerUps :: GameState -> GameState
updatePowerUps gs@Game {powerUps = powerUps'} = gs {powerUps = map (updateTime) powerUps'}

updateGameState :: GameState -> GameState
updateGameState = ckeckInput . destroyObjects . updateAirplanes . updateProjectiles . updatePowerUps

pauseMenu :: GameState -> GameState
pauseMenu gs@Game {status = _status} = gs {status = toggleStatus}
  where
    toggleStatus :: Status
    toggleStatus = case _status of
      InMenu -> InGame
      InGame -> InMenu
