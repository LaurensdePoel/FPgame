module Enemy where

import Model

-- TODO Naming refactor
-- TODO values in Config.hs

-- | Check if the Airplane has reaced its seeked destination
isDestinationReached :: Airplane -> Bool
isDestinationReached Airplane {airplanePos = _currentPosition, airplaneDestinationPos = _destination} =
  abs (_destination - _currentPosition) < (0.2, 0.2)

-- | gets the position of the closest player based on the fighters current position
closestPlayer :: Airplane -> [Airplane] -> Position
closestPlayer Airplane {airplanePos = _currentPos} [] = _currentPos
closestPlayer Airplane {airplanePos = _currentPos} (player : ps) =
  foldr (\Airplane {airplanePos = _playerPos} r -> if isCloser _playerPos r then _playerPos else r) (airplanePos player) ps
  where
    isCloser :: Position -> Position -> Bool
    isCloser newPos pos = distance newPos < distance pos

    distance :: Position -> Float
    distance (x, y) = sqrt (((x - fst _currentPos) ** 2) + ((y - snd _currentPos) ** 2))

enemyBehaviourHandler :: GameState -> GameState
enemyBehaviourHandler gs@GameState {players = _players, enemies = _enemies} = gs {enemies = updatedEnemies}
  where
    updatedEnemies = map updateBehaviour _enemies
    updateBehaviour enemy = case airplaneType enemy of
      Fighter
        | isDestinationReached updatedAirplane -> updatedAirplane {airplaneDestinationPos = (300, 300)} -- should be randomly generated
        | otherwise -> updatedAirplane
      Kamikaze -> updatedAirplane {airplaneDestinationPos = closestPlayer updatedAirplane _players} -- should be center player pos
      _ -> enemy {airplaneVelocity = (minVel,0)} 
      where
        updatedAirplane = enemy {airplaneVelocity = updatedVelocity (airplaneVelocity enemy) (airplanePos enemy) (airplaneDestinationPos enemy)}

        updatedVelocity :: Velocity -> Position -> Position -> Velocity
        updatedVelocity (x, y) (cx, cy) (dx, dy) = (maxMin minVel maxVel (x + direction cx dx), maxMin minVel maxVel (y + direction cy dy))

        direction :: Float -> Float -> Float
        direction pos des = case signum (des - pos) of
          (-1) -> (-0.25)
          1 -> 0.25
          _ -> 0.0

        maxMin :: Float -> Float -> Float -> Float
        maxMin minValue maxValue value = max minValue (min maxValue value)

        minVel, maxVel :: Float
        minVel = fst $ airplaneMaxVelocity enemy
        maxVel = snd $ airplaneMaxVelocity enemy