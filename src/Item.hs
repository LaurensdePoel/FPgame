{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Item where

import Assets
import Config as C
import Model

-- | PowerUpTypes instance of enum
instance Enum PowerUpTypes where
  toEnum :: Int -> PowerUpTypes
  toEnum 0 = HealthPack C.healthPackValue
  toEnum _ = PowerPack C.powerPackValue

  fromEnum :: PowerUpTypes -> Int
  fromEnum (HealthPack _) = 0
  fromEnum (PowerPack _) = 1

-- | PowerUpTypes instance of bound
instance Bounded PowerUpTypes where
  minBound :: PowerUpTypes
  minBound = HealthPack C.healthPackValue
  maxBound :: PowerUpTypes
  maxBound = PowerPack C.powerPackValue