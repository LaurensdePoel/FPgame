module Enemy where

import Config as C
import Model

-- | Check if the Airplane has reached its destination
isDestinationReached :: Airplane -> Bool
isDestinationReached Airplane {airplanePos = _currentPosition, airplaneDestinationPos = _destination} =
  abs (_destination - _currentPosition) < C.destinationErrorMargin

-- | Gets the position of the closest player based on the current position of the enemy
closestPlayer :: Airplane -> [Airplane] -> Position
closestPlayer Airplane {airplanePos = _currentPos} [] = _currentPos
closestPlayer
  Airplane {airplanePos = _currentPos}
  (Airplane {airplanePos = _playerPos, airplaneSize = _playerSize} : players') = foldr updateClosestPlayer (centerPosition _playerPos _playerSize) players'
    where
      centerPosition :: Position -> Size -> Position
      centerPosition (posX, posY) (sizeX, sizeY) = (posX + (sizeX * 0.5), posY - (sizeY * 0.5))
      updateClosestPlayer :: Airplane -> Position -> Position
      updateClosestPlayer Airplane {airplanePos = _pos, airplaneSize = _size} rest =
        case isCloser position rest of
          True -> position
          False -> rest
        where
          isCloser :: Position -> Position -> Bool
          isCloser newPos pos = distance newPos < distance pos
          distance :: Position -> Float
          distance (x, y) = sqrt (((x - fst _currentPos) ** 2) + ((y - snd _currentPos) ** 2))
          position :: Position
          position = centerPosition _pos _size

-- | Handles the behaviour of different enemies
enemyBehaviourHandler :: GameState -> GameState
enemyBehaviourHandler gs@GameState {players = _players, enemies = _enemies} = gs {enemies = updatedEnemies}
  where
    updatedEnemies :: [Airplane]
    updatedEnemies = map updateBehaviour _enemies

    updateBehaviour :: Airplane -> Airplane
    updateBehaviour enemy@Airplane {airplaneType = _type, airplaneMaxVelocity = _maxVelocity} = case _type of
      Fighter
        | isDestinationReached updatedAirplane -> updatedAirplane {airplaneDestinationPos = (300,300)} -- should be randomly generated
        | otherwise -> updatedAirplane
      Kamikaze -> updatedAirplane {airplaneDestinationPos = closestPlayer updatedAirplane _players}
      _ -> enemy {airplaneVelocity = (minVelocity, 0)}
      where
        updatedAirplane :: Airplane
        updatedAirplane = enemy {airplaneVelocity = updatedVelocity (airplaneVelocity enemy) (airplanePos enemy) (airplaneDestinationPos enemy)}

        updatedVelocity :: Velocity -> Position -> Position -> Velocity
        updatedVelocity (velocityX, velocityY) (currentPosX, currentPosY) (destinationX, destinationY) =
          (maxMin minVelocity maxVelocity (velocityX + direction currentPosX destinationX), maxMin minVelocity maxVelocity (velocityY + direction currentPosY destinationY)) -- TODO: can this be written cleaner
        direction :: Float -> Float -> Float
        direction position destination = case signum (destination - position) of
          (-1) -> (- C.behaviourVelocitySteps)
          1 -> C.behaviourVelocitySteps
          _ -> 0.0

        maxMin :: Float -> Float -> Float -> Float
        maxMin minValue maxValue value = max minValue (min maxValue value) -- TODO: move maxMin to generic file as it is used in multiple files
        minVelocity, maxVelocity :: Float
        minVelocity = fst _maxVelocity
        maxVelocity = snd _maxVelocity