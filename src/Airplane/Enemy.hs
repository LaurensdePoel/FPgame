module Enemy where

import Airplane
import Config as C
import Model
import Updateable

-- | Check if the Airplane has reached its destination
isDestinationReached :: Airplane -> Bool
isDestinationReached Airplane {airplanePos = _currentPosition, airplaneDestinationPos = _destination} =
  abs (_destination - _currentPosition) < C.destinationErrorMargin

-- | Gets the position of the closest player based on the current position of the enemy
closestPlayer :: Airplane -> [Airplane] -> Position
closestPlayer Airplane {airplanePos = _currentPos} [] = _currentPos
closestPlayer
  Airplane {airplanePos = _currentPos}
  (player : players') = foldr updateClosestPlayer (getCenterPosition player) players'
    where
      updateClosestPlayer :: Airplane -> Position -> Position
      updateClosestPlayer airplane rest =
        case isCloser position rest of
          True -> position
          False -> rest
        where
          isCloser :: Position -> Position -> Bool
          isCloser newPos pos = distance newPos < distance pos
          distance :: Position -> Float
          distance (x, y) = sqrt (((x - fst _currentPos) ** 2) + ((y - snd _currentPos) ** 2))
          position :: Position
          position = getCenterPosition airplane

-- | Handles the behaviour of different enemies
enemyBehaviourHandler :: [Position] -> GameState -> GameState
enemyBehaviourHandler randomPoints gs@GameState {players = _players, enemies = _enemies} = gs {enemies = updatedEnemies}
  where
    updatedEnemies :: [Airplane]
    -- updatedEnemies = map updateBehaviour _enemies
    updatedEnemies = snd $ foldr (\enemy (randPoints, enemiesList) -> let (updatedEnemy, updatedRandPoints) = updateBehaviour enemy randPoints in (updatedRandPoints, updatedEnemy : enemiesList)) (randomPoints, []) _enemies

    getPointOfList :: [Position] -> (Position, [Position])
    getPointOfList positions
      | length positions <= 1 = (head positions, positions)
      | otherwise = (head positions, tail positions)

    updateBehaviour :: Airplane -> [Position] -> (Airplane, [Position])
    updateBehaviour enemy@Airplane {airplaneType = _type, airplaneMaxVelocity = _maxVelocity} points =
      case _type of
        Fighter
          | isDestinationReached updatedAirplane -> (updatedAirplane {airplaneDestinationPos = fst newDestination}, snd newDestination)
          | otherwise -> (updatedAirplane, points)
        Kamikaze -> (updatedAirplane {airplaneDestinationPos = closestPlayer updatedAirplane _players}, points)
        _ -> (enemy {airplaneVelocity = (fst _maxVelocity, 0)}, points)
      where
        updatedAirplane :: Airplane
        updatedAirplane = enemy {airplaneVelocity = updatedVelocity (airplaneVelocity enemy) (airplanePos enemy) (airplaneDestinationPos enemy)}

        newDestination = getPointOfList points

        updatedVelocity :: Velocity -> Position -> Position -> Velocity
        updatedVelocity (velocityX, velocityY) (currentPosX, currentPosY) (destinationX, destinationY) =
          (minMax _maxVelocity (velocityX + direction currentPosX destinationX), minMax _maxVelocity (velocityY + direction currentPosY destinationY)) -- TODO: can this be written cleaner
        direction :: Float -> Float -> Float
        direction position destination = case signum (destination - position) of
          (-1) -> (-C.behaviourVelocitySteps)
          1 -> C.behaviourVelocitySteps
          _ -> 0.0