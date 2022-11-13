module Player where

import Airplane
import Config as C
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact
import Helper
import Model

-- | Apply or remove powerUp effect on/from the airplane
powerUpEffect :: Bool -> Airplane -> PowerUp -> Airplane
powerUpEffect
  applyEffect
  player@Airplane {fireRate = _fireRate, airplaneHealth = _health, airplanePowerUps = _airplanePowerUps}
  powerUp@PowerUp {powerUpType = puType} =
    case puType of
      PowerPack x -> case _fireRate of
        Single x' -> player {fireRate = Single $ updateBuffer x' x, airplanePowerUps = airplanePowerUps}
        Burst x' -> player {fireRate = Burst $ updateBuffer x' x, airplanePowerUps = airplanePowerUps}
      HealthPack x -> player {airplaneHealth = _health + x}
    where
      airplanePowerUps :: [PowerUp]
      airplanePowerUps = powerUp {powerUpState = PickedUp} : _airplanePowerUps

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
addVelocityBasedOnKey key airplane@Airplane {airplaneType = _planeType, airplaneMaxVelocity = _maxVelocity} =
  case _planeType of
    Player1
      | key == Char 'w' -> airplane {airplaneVelocity = add (0, C.velocityStep)}
      | key == Char 'a' -> airplane {airplaneVelocity = add (-C.velocityStep, 0)}
      | key == Char 's' -> airplane {airplaneVelocity = add (0, -C.velocityStep)}
      | key == Char 'd' -> airplane {airplaneVelocity = add (C.velocityStep, -0)}
      | otherwise -> airplane
    Player2
      | key == SpecialKey KeyUp -> airplane {airplaneVelocity = add (0, C.velocityStep)}
      | key == SpecialKey KeyLeft -> airplane {airplaneVelocity = add (-C.velocityStep, 0)}
      | key == SpecialKey KeyDown -> airplane {airplaneVelocity = add (0, -C.velocityStep)}
      | key == SpecialKey KeyRight -> airplane {airplaneVelocity = add (C.velocityStep, -0)}
      | otherwise -> airplane
    _ -> airplane
  where
    add :: Velocity -> Velocity
    add vel = checkMinMax (airplaneVelocity airplane + vel)

    checkMinMax :: Velocity -> Velocity
    checkMinMax (vX, vY) = (minMax _maxVelocity vX, minMax _maxVelocity vY)
