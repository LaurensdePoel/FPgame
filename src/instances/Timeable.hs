{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Timeable type class
module Timeable where

import Animateable (Animateable (nextSprite))
import Config as C
import Model
  ( Airplane (..),
    AirplaneGun (..),
    FireRate (..),
    Level (..),
    Particle (..),
    PowerUp (..),
    PowerUpState (..),
    Sprites (..),
    Wave (..),
  )

-------------------------------------------------

-- * Timeable class

-------------------------------------------------

class Timeable a where
  -- | Updates the timer of a Timeable
  updateTime :: a -> a

  -- | Checks if the timer is finished
  readyToExecute :: a -> Bool

  -- | Applies function on the Timeable, when it is ready to execute
  applyOnExecute :: (a -> a) -> a -> a
  applyOnExecute f a =
    case readyToExecute updatedA of
      True -> f updatedA
      False -> updatedA
    where
      updatedA = updateTime a

-------------------------------------------------

-- * Instances

-------------------------------------------------

instance Timeable Airplane where
  -- \| Updates the timeLastShot of an airplane
  updateTime :: Airplane -> Airplane
  updateTime airplane@Airplane {fireRate = _fireRate, timeLastShot = _timeLastShot} =
    case _fireRate of
      Single time
        | _timeLastShot > time -> airplane {timeLastShot = C.resetTime}
        | otherwise -> airplane {timeLastShot = _timeLastShot + C.timeInterval}
      Burst time
        | _timeLastShot > time -> airplane {timeLastShot = C.resetTime}
        | otherwise -> airplane {timeLastShot = _timeLastShot + C.timeInterval}

  -- \| Checks if the airplane is ready to shoot
  readyToExecute :: Airplane -> Bool
  readyToExecute Airplane {timeLastShot = _time, airplaneGun = _gunType}
    | _time == C.resetTime && _gunType /= None = True
    | otherwise = False

instance Timeable PowerUp where
  -- \| Updates the timer of a powerUp
  updateTime :: PowerUp -> PowerUp
  updateTime powerUp@PowerUp {powerUpState = _state, timeUntilDespawn = _despawnTime, powerUpDuration = _duration, powerUpSprites = _sprites} =
    case _state of
      PickedUp -> powerUp {powerUpDuration = max C.resetTime (_duration - C.timeInterval), powerUpSprites = updatedSprites}
      WorldSpace -> powerUp {timeUntilDespawn = max C.resetTime (_despawnTime - C.timeInterval), powerUpSprites = updatedSprites}
    where
      updatedSprites = applyOnExecute nextSprite _sprites

  -- \| Checks if the powerUp is ready to be removed
  readyToExecute :: PowerUp -> Bool
  readyToExecute PowerUp {powerUpState = _state, timeUntilDespawn = _despawnTime, powerUpDuration = _duration} =
    case _state of
      PickedUp
        | _duration <= C.resetTime -> True
        | otherwise -> False
      WorldSpace
        | _despawnTime <= C.resetTime -> True
        | otherwise -> False

instance Timeable Particle where
  -- \| Updates the timer of a particle
  updateTime :: Particle -> Particle
  updateTime particle@Particle {particleTimer = _timer} = particle {particleTimer = max C.resetTime (_timer - C.timeInterval)}

  -- \| Checks if the particle interval timer is zero
  readyToExecute :: Particle -> Bool
  readyToExecute Particle {particleTimer = timer}
    | timer <= C.resetTime = True
    | otherwise = False

instance Timeable Sprites where
  -- \| Updates the timer of the Sprites
  updateTime :: Sprites -> Sprites
  updateTime sprites@Sprites {spritesTimer = _timer} = sprites {spritesTimer = max C.resetTime (_timer - C.timeInterval)}

  -- \| Checks if the sprites interval timer is zero
  readyToExecute :: Sprites -> Bool
  readyToExecute Sprites {spritesTimer = _timer}
    | _timer <= C.resetTime = True
    | otherwise = False

instance Timeable Level where
  -- \| Updates the wave timer of a level
  updateTime :: Level -> Level
  updateTime level@Level {waves = _waves, levelBackground = _background} = level {waves = updateCurrentWave _waves}
    where
      updateCurrentWave :: [Wave] -> [Wave]
      updateCurrentWave [] = []
      updateCurrentWave (x : xs) = updateTime x : xs

  -- \| Checks if the next wave is ready to spawn
  readyToExecute :: Level -> Bool
  readyToExecute Level {waves = []} = False
  readyToExecute Level {waves = x : _} = readyToExecute x

instance Timeable Wave where
  -- \| Updates the wave timer
  updateTime :: Wave -> Wave
  updateTime wave@Wave {waveTimer = _waveTimer} = wave {waveTimer = max C.resetTime (_waveTimer - C.timeInterval)}

  -- \| Checks if the wave timer is zero
  readyToExecute :: Wave -> Bool
  readyToExecute Wave {waveTimer = _waveTimer}
    | _waveTimer <= C.resetTime = True
    | otherwise = False
