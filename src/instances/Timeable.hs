{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Timeable type class
module Timeable where

import Animateable
import Model

-------------------------------------------------
-- Timeable class
-------------------------------------------------

class Timeable a where
  updateTime :: a -> a
  readyToExecute :: a -> Bool
  onChange :: (a -> a) -> a -> a
  onChange f a = let updatedA = updateTime a in if readyToExecute updatedA then f updatedA else updatedA

-------------------------------------------------
-- Helper functions
-------------------------------------------------

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Timeable Airplane where
  updateTime :: Airplane -> Airplane
  updateTime airplane'@Airplane {fireRate = fireRate', timeLastShot = time} = case fireRate' of
    Single time'
      | time > time' -> airplane' {timeLastShot = 0.0}
      | otherwise -> airplane' {timeLastShot = time + 1.0}
    Burst time'
      | time > time' -> airplane' {timeLastShot = 0.0}
      | otherwise -> airplane' {timeLastShot = time + 1.0}

  readyToExecute :: Airplane -> Bool
  readyToExecute Airplane {timeLastShot = time}
    | time == 0.0 = True
    | otherwise = False

instance Timeable PowerUp where
  updateTime :: PowerUp -> PowerUp
  updateTime powerUp@PowerUp {powerUpState = state, timeUntilDespawn = despawnTime, powerUpDuration = duration} =
    case state of
      PickedUp -> powerUp {powerUpDuration = max 0.0 (duration - 1.0), powerUpSprites = updatedSprites}
      WorldSpace -> powerUp {timeUntilDespawn = max 0.0 (despawnTime - 1.0), powerUpSprites = updatedSprites}
    where
      updatedSprites = onChange nextSprite $ powerUpSprites powerUp

  readyToExecute :: PowerUp -> Bool
  readyToExecute PowerUp {powerUpState = state, timeUntilDespawn = despawnTime, powerUpDuration = duration} =
    case state of
      PickedUp
        | duration <= 0.0 -> True
        | otherwise -> False
      WorldSpace
        | despawnTime <= 0.0 -> True
        | otherwise -> False

instance Timeable Particle where
  updateTime :: Particle -> Particle
  updateTime p@Particle {particleTimer = timer} = p {particleTimer = max 0 (timer - 1)}

  readyToExecute :: Particle -> Bool
  readyToExecute Particle {particleTimer = timer}
    | timer <= 0 = True
    | otherwise = False

instance Timeable Sprites where
  updateTime :: Sprites -> Sprites
  updateTime sprites = sprites {spritesTimer = max 0 (spritesTimer sprites - 1)}

  readyToExecute :: Sprites -> Bool
  readyToExecute Sprites {spritesTimer = timer}
    | timer <= 0 = True
    | otherwise = False