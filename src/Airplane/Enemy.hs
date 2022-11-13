module Enemy where

import Config as C
import Helper (minMax)
import Model
import Updateable (Updateable (getClosestPosition))

-- | Check if the Airplane has reached its destination
isDestinationReached :: Airplane -> Bool
isDestinationReached Airplane {airplanePos = _currentPosition, airplaneDestinationPos = _destination} =
  abs (_destination - _currentPosition) < C.destinationErrorMargin

-- | Handles the behaviour of different enemies
enemyBehaviourHandler :: [Position] -> GameState -> GameState
enemyBehaviourHandler randomPoints gs@GameState {players = _players, enemies = _enemies} = gs {enemies = updatedEnemies}
  where
    updatedEnemies :: [Airplane]
    updatedEnemies =
      snd $
        foldr
          ( \enemy (randPoints, enemiesList) ->
              let (updatedEnemy, updatedRandPoints) = updateBehaviour enemy randPoints
               in (updatedRandPoints, updatedEnemy : enemiesList)
          )
          (randomPoints, [])
          _enemies

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
        Kamikaze -> (updatedAirplane {airplaneDestinationPos = getClosestPosition updatedAirplane _players}, points)
        _ -> (enemy {airplaneVelocity = (fst _maxVelocity, 0)}, points)
      where
        updatedAirplane :: Airplane
        updatedAirplane = enemy {airplaneVelocity = updatedVelocity (airplaneVelocity enemy) (airplanePos enemy) (airplaneDestinationPos enemy)}

        newDestination :: (Position, [Position])
        newDestination = getPointOfList points

        updatedVelocity :: Velocity -> Position -> Position -> Velocity
        updatedVelocity (velocityX, velocityY) (currentPosX, currentPosY) (destinationX, destinationY) =
          ( minMax _maxVelocity (velocityX + direction currentPosX destinationX),
            minMax _maxVelocity (velocityY + direction currentPosY destinationY)
          )

        direction :: Float -> Float -> Float
        direction position destination = case signum (destination - position) of
          (-1) -> (-C.behaviourVelocitySteps)
          1 -> C.behaviourVelocitySteps
          _ -> 0.0