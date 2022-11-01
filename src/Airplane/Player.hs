module Player where

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact
import Model

-- Applies or removes powerUp effect from the airplane
-- TODO REFACTOR NAMES
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
    velocityStep = 0.6
