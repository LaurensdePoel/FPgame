{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Damageable type class
module Damageable where

import Model

-------------------------------------------------

-- * Timeable class

-------------------------------------------------

class Damageable a where
  -- | Applies damage to the Damageable
  takeDamage :: Int -> a -> a

-------------------------------------------------

-- * Instances

-------------------------------------------------

instance Damageable Airplane where
  -- \| Apply damage to the airplane
  takeDamage :: Int -> Airplane -> Airplane
  takeDamage damage airplane@Airplane {airplaneHealth = _health} = airplane {airplaneHealth = max 0 (_health - damage)}

instance Damageable Projectile where
  -- \| Apply damage to the projectile
  takeDamage :: Int -> Projectile -> Projectile
  takeDamage damage projectile@Projectile {projectileHealth = _health} = projectile {projectileHealth = max 0 (_health - damage)}