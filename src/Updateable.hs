{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Updateable type class
module Updateable where

import Data.Maybe
import Model

-------------------------------------------------
-- Updateable class
-------------------------------------------------

class Updateable a where
  move :: a -> a

  destroy :: a -> Maybe a

  destroyFromList :: [a] -> [a]
  destroyFromList = mapMaybe destroy

  damage :: Int -> a -> a

-------------------------------------------------
-- Helper functions
-------------------------------------------------

updatePosition :: Position -> Velocity -> Position
updatePosition (pX, pY) (vX, vY) = (pX + vX, pY + vY)

updateVelocity :: Velocity -> Velocity
updateVelocity (x, y) = (update x, update y)
  where
    update z
      | signum z == 1 = if z > 0.2 then z - 0.2 else 0.0
      | otherwise = if z < -0.2 then z + 0.2 else 0.0

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Updateable Airplane where
  move :: Airplane -> Airplane
  -- move airplane@Airplane {airplanePos = p, airplaneVelocity = v} = airplane {airplanePos = updatePosition p v}
  move airplane@Airplane {airplanePos = p, airplaneVelocity = v} = airplane {airplanePos = updatePosition p v, airplaneVelocity = updateVelocity v}

  destroy :: Airplane -> Maybe Airplane
  destroy airplane@Airplane {airplaneHealth = h}
    | h <= 0 = Nothing
    | otherwise = Just airplane

  damage :: Int -> Airplane -> Airplane
  damage d airplane@Airplane {airplaneHealth = h} = airplane {airplaneHealth = newHealth}
    where
      damage = h - d
      newHealth
        | damage < 0 = 0
        | otherwise = damage

instance Updateable Projectile where
  move :: Projectile -> Projectile
  move projectile@Projectile {projectilePos = p, projectileVelocity = v} = projectile {projectilePos = updatePosition p v}

  destroy :: Projectile -> Maybe Projectile
  destroy projectile@Projectile {projectileHealth = h}
    | h <= 0 = Nothing
    | otherwise = Just projectile

  damage :: Int -> Projectile -> Projectile
  damage d projectile@Projectile {projectileHealth = h} = projectile {projectileHealth = newHealth}
    where
      damage = h - d
      newHealth
        | damage < 0 = 0
        | otherwise = damage
