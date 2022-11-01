module Player where

import Config as C
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact
import Model

-- Applies or removes powerUp effect from the airplane
-- TODO Naming refactor
-- TODO REWRITE FUNCTIONS
-- TODO values in Config.hs

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

-- | Loops trough all keys and if key has effect on the plane update it's velocity
updatePlayerVelocity :: S.Set Key -> Airplane -> Airplane
updatePlayerVelocity activeKeys airplane =
  foldr f e activeKeys
  where
    f = addVelocityBasedOnKey
    e = airplane

-- | If key affects current airplane change velocity
addVelocityBasedOnKey :: Key -> Airplane -> Airplane
addVelocityBasedOnKey key airplane@Airplane {airplaneType = _planeType, airplaneMaxVelocity = (minVel, maxVel)} =
  case _planeType of
    Player1
      | key == Char 'w' -> airplane {airplaneVelocity = add (0, C.velocityStep)}
      | key == Char 'a' -> airplane {airplaneVelocity = add (- C.velocityStep, 0)}
      | key == Char 's' -> airplane {airplaneVelocity = add (0, - C.velocityStep)}
      | key == Char 'd' -> airplane {airplaneVelocity = add (C.velocityStep, -0)}
      | otherwise -> airplane
    Player2
      | key == SpecialKey KeyUp -> airplane {airplaneVelocity = add (0, C.velocityStep)}
      | key == SpecialKey KeyLeft -> airplane {airplaneVelocity = add (- C.velocityStep, 0)}
      | key == SpecialKey KeyDown -> airplane {airplaneVelocity = add (0, - C.velocityStep)}
      | key == SpecialKey KeyRight -> airplane {airplaneVelocity = add (C.velocityStep, -0)}
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
