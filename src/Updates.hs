-- | This module defines the Updates of entities in the game
module Updates where

import Updateable
import Collidable
import Model

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game
  ( Key (Char),
  )


-- Updates velocity based on pressed keys. Foldr loops trough every key and add new velocity to current Airplane
updatePlayerVelocity :: S.Set Key -> Airplane -> Airplane
updatePlayerVelocity activeKeys airplane =
  foldr f e activeKeys
    where
      f = addVelocityBasedOnKey
      e = airplane

-- If key affects velocity of the player update the current velocity
addVelocityBasedOnKey :: Key -> Airplane -> Airplane
addVelocityBasedOnKey key airplane
  -- | S.member (SpecialKey KeyUp) activeKeys =
  --   airplane {airplanePos = (0, 0)}
  | key == Char 'w' = airplane {airplaneVelocity = add (0, velocityStep)}
  | key == Char 'a' = airplane {airplaneVelocity = add (-velocityStep, 0)}
  | key == Char 's' = airplane {airplaneVelocity = add (0, -velocityStep)}
  | key == Char 'd' = airplane {airplaneVelocity = add (velocityStep, -0)}
  | otherwise = airplane
    where
      add :: Velocity -> Velocity
      add vel = checkMinMax (airplaneVelocity airplane + vel)
      checkMinMax :: Velocity -> Velocity
      checkMinMax orignalVel@(vX,vY)
        | vX < minVel = (minVel,vY)
        | vY < minVel = (vX,minVel)
        | vX > maxVel = (maxVel,vY)
        | vY > maxVel = (vX,maxVel)
        |otherwise = orignalVel
      --TODO move values below to special HS file those values are base parameters
      minVel = -12.0
      maxVel = 12.0
      velocityStep = 0.6


updateFireRate :: Airplane -> Airplane
updateFireRate airplane@Airplane {fireRate = r, timeLastShot = t} = case r of
  Single x
    | t > x -> airplane {timeLastShot = 0.0}
    | otherwise -> airplane {timeLastShot = t + 1.0}
  Burst x
    | t > x -> airplane {timeLastShot = 0.0}
    | otherwise -> airplane {timeLastShot = t + 1.0}

readyToFire :: Airplane -> Bool
readyToFire Airplane {timeLastShot = t}
  | t == 0.0 = True
  | otherwise = False

shoot :: Airplane -> [Projectile]
shoot Airplane {airplanePos = p@(x, y), fireRate = r, airplaneProjectile = projectile} = case r of
  Single x -> [projectile {projectilePos = p}]
  Burst x -> [projectile {projectilePos = (x - px, y)}, projectile {projectilePos = p}, projectile {projectilePos = (x + px, y)}]
    where
      (Size (px, _)) = projectileSize projectile

-- TODO: handle airplane collides screenbox
updateAirplanes :: GameState -> GameState
-- updateAirplanes gs@GameState {players = players, enemies = enemies, projectiles = projectiles} = gs {players = players', enemies = enemies', projectiles = newProjectiles ++ projectiles}  -- maybe moves, update fire rate, maybe add projectile to projectile list, take damage if collides with enemy plane(also other object) , maybe destroy
updateAirplanes gs@Game {players = players, enemies = enemies, projectiles = projectiles, pressedKeys = pressedKeys} = gs {players = updatedPlayers, enemies = updatedEnemies, projectiles = newProjectiles ++ projectiles} -- maybe moves, update fire rate, maybe add projectile to projectile list, take damage if collides with enemy plane(also other object) , maybe destroy
  where
    updatedPlayers :: [Airplane]
    updatedPlayers = map (updateFireRate . move . updatePlayerVelocity pressedKeys) players

    updatedEnemies :: [Airplane]
    updatedEnemies = map (updateFireRate . move) enemies

    newProjectiles :: [Projectile]
    newProjectiles = concatMap shoot $ filter readyToFire (updatedPlayers ++ updatedEnemies)

-- (players', enemies') = checkCollisions updatedPlayers updatedEnemies

-- checkCollisions :: => [Airplane] -> [Airplane] -> ([Airplane],[Airplane]) -- ToDo: makes this function better/ less ugly. Only works when there are two players
-- checkCollisions pps pes = ([p1,p2], pes'')       -- unzip . map (`checkCollisions'` pes) pps  -- [(Airplane,[Airplane]), (Airplane,[Airplane])] -- [[Airplane],[Airplane]]
--     where
--         (p1, pes') = checkCollisions' (head pps) pes
--         (p2, pes'') = checkCollisions' (tail pps) pes'

--         checkCollisions' :: Airplane -> [Airplane] -> (Airplane,[Airplane])
--         checkCollisions' _ [] = ([],[])
--         checkCollisions' pp (pe:pes) = case pp `collides` pe of
--                                                 True -> let (pp',pes') = checkCollisions' pp pes in ((damage (airplaneHealth pp) pp'), (airplaneHealth pe):pes')
--                                                 False -> let (pp',pes') = checkCollisions' pp pes in (pp',(pe:pes'))

-- ToDO: handle all updates ||||| for now it only checks if projectile is still on screen
updateProjectiles :: GameState -> GameState
updateProjectiles gs@Game {window = w, projectiles = projectiles} = gs {projectiles = (filter (`collides` w) (map move projectiles))}

-- destroys all objects with zero health
destroyObjects :: GameState -> GameState
destroyObjects gs@Game {players = players, enemies = enemies, projectiles = projectiles} = gs {players = destroyFromList players, enemies = destroyFromList enemies, projectiles = destroyFromList projectiles}

updateGameState :: GameState -> GameState
updateGameState = destroyObjects . updateProjectiles . updateAirplanes
