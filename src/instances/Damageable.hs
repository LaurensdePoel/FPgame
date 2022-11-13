{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Damageable type class
module Damageable where

import Config as C
import Model

-------------------------------------------------

-- * Timeable class

-------------------------------------------------

class Damageable a where
  -- | Applies damage to the Damageable
  takeDamage :: Int -> a -> a

  -- Applies damage to both Damageable's based on each others damage
  damageBoth :: Damageable b => a -> b -> (a, b)
  damageBoth a b = (takeDamage (getDamagePoints b) a, takeDamage (getDamagePoints a) b)

  -- Returns the damage points the Damageable
  getDamagePoints :: a -> Int

-------------------------------------------------

-- * Instances

-------------------------------------------------

instance Damageable Airplane where
  -- \| Apply damage to the airplane
  takeDamage :: Int -> Airplane -> Airplane
  takeDamage damage airplane@Airplane {airplaneHealth = _health} = airplane {airplaneHealth = max 0 (_health - damage)}

  -- \| Get damage points from the airplane (which is its current health)
  getDamagePoints :: Airplane -> Int
  getDamagePoints = airplaneHealth

instance Damageable Projectile where
  -- \| Apply damage to the projectile
  takeDamage :: Int -> Projectile -> Projectile
  takeDamage damage projectile@Projectile {projectileHealth = _health, projectileType = _type} = projectile {projectileHealth = max 0 (_health - damage)}

  -- \| Get damage points of the projectile
  getDamagePoints :: Projectile -> Int
  getDamagePoints Projectile {projectileType = _type, projectileDamage = _damage} = updatedDamage
    where
      updatedDamage :: Int
      updatedDamage
        | _type == DoubleGun = _damage * C.damageMultiplier
        | otherwise = _damage
