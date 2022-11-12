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

  -- Returns the damage points the Damageable
  getDamagePoints :: a -> Int

-------------------------------------------------

-- * Instances

-------------------------------------------------

instance Damageable Airplane where
  -- \| Apply damage to the airplane
  takeDamage :: Int -> Airplane -> Airplane
  takeDamage damage airplane@Airplane {airplaneHealth = _health} = airplane {airplaneHealth = max 0 (_health - damage)}

  damageBoth :: Damageable b => Airplane -> b -> (Airplane, b)
  damageBoth airplane other = (takeDamage (getDamagePoints other) airplane, takeDamage (getDamagePoints airplane) other)

  getDamagePoints :: Airplane -> Int
  getDamagePoints = airplaneHealth

instance Damageable Projectile where
  -- \| Apply damage to the projectile
  takeDamage :: Int -> Projectile -> Projectile
  takeDamage damage projectile@Projectile {projectileHealth = _health, projectileType = _type} = projectile {projectileHealth = max 0 (_health - damage)}

  damageBoth :: Damageable b => Projectile -> b -> (Projectile, b)
  damageBoth projectile other = (takeDamage (getDamagePoints other) projectile, takeDamage (getDamagePoints projectile) other)

  getDamagePoints :: Projectile -> Int
  getDamagePoints projectile@Projectile {projectileType = _type} = updatedDamage
    where
      damage = projectileDamage projectile
      updatedDamage
        | _type == DoubleGun = damage * C.damageMultiplier
        | otherwise = damage
