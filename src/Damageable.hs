{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Damageable type class
module Damageable where

import Model

-------------------------------------------------
-- Timeable class
-------------------------------------------------

class Damageable a where
  damage :: Int -> a -> a

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Damageable Airplane where
  damage :: Int -> Airplane -> Airplane
  damage damage' airplane@Airplane {airplaneHealth = health} = airplane {airplaneHealth = max 0 (health - damage')}

instance Damageable Projectile where
  damage :: Int -> Projectile -> Projectile
  damage damage' projectile@Projectile {projectileHealth = health} = projectile {projectileHealth = max 0 (health - damage')}