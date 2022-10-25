{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Timeable type class
module Timeable where

import Model

-------------------------------------------------
-- Timeable class
-------------------------------------------------

class Timeable a where
  updateTime :: a -> a
  readyToExecute :: a -> Bool

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
  updateTime powerUp@PowerUp {powerUpState = state, timeUntilDespawn = despawnTime, powerUpDuration = duration} = case state of
    PickedUp -> powerUp {powerUpDuration = max 0.0 (duration - 1.0)}
    WorldSpace -> powerUp {timeUntilDespawn = max 0.0 (despawnTime - 1.0)}

  readyToExecute :: PowerUp -> Bool
  readyToExecute PowerUp {powerUpState = state, timeUntilDespawn = despawnTime, powerUpDuration = duration} = True -- case state of
  -- PickedUp
  --   | powerUpDuration <= 0.0 -> True
  --   | otherwise -> False
  -- WorldSpace
  --   | despawnTime <= 0.0 -> True
  --   | otherwise -> False