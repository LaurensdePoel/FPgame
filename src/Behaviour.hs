module Behaviour where

import Model

isDestinationReached :: Airplane -> Bool
isDestinationReached Airplane {airplanePos = currentPosition, airplaneDestinationPos = destination} = abs (destination - currentPosition) < (0.2, 0.2)

closestPlayer :: Airplane -> [Airplane] -> Position
closestPlayer Airplane {airplanePos = currentPos} (p : ps) = foldr (\Airplane {airplanePos = _p} r -> if isCloser _p r then _p else r) (airplanePos p) ps
  where
    isCloser :: Position -> Position -> Bool
    isCloser newPos pos = distance newPos < distance pos

    distance :: Position -> Float
    distance (x, y) = sqrt (((x - fst currentPos) ** 2) + ((y - snd currentPos) ** 2))

enemyBehaviourHandler :: GameState -> GameState
enemyBehaviourHandler gs@Game {players = _players, enemies = _enemies} = gs {enemies = updatedEnemies}
  where
    updatedEnemies = map updateBehaviour _enemies
    updateBehaviour enemy = case airplaneType enemy of
      Fighter
        | isDestinationReached updatedAirplane -> updatedAirplane {airplaneDestinationPos = (300, 300)} -- should be randomly generated
        | otherwise -> updatedAirplane
      Kamikaze -> updatedAirplane {airplaneDestinationPos = closestPlayer updatedAirplane _players}
      _ -> enemy
      where
        updatedAirplane = enemy {airplaneVelocity = updatedVelocity (airplaneVelocity enemy) (airplanePos enemy) (airplaneDestinationPos enemy)}

        updatedVelocity :: Velocity -> Position -> Position -> Velocity
        updatedVelocity (x, y) (cx, cy) (dx, dy) = (maxMin minVel maxVel (x + direction cx dx), maxMin minVel maxVel (y + direction cy dy))

        direction :: Float -> Float -> Float
        direction pos des = case signum (des - pos) of
          (-1) -> (-0.25)
          0 -> 0.0
          1 -> (0.25)

        maxMin :: Float -> Float -> Float -> Float
        maxMin minValue maxValue value = max minValue (min maxValue value)

        minVel = fst $ airplaneMaxVelocity enemy
        maxVel = snd $ airplaneMaxVelocity enemy