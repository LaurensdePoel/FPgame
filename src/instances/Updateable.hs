{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Updateable type class
module Updateable where

import Config as C
import Data.Maybe
import Model

-- TODO Naming refactor

-------------------------------------------------

-- * Updateable class

-------------------------------------------------

class Updateable a where
  move :: a -> a

  destroy :: a -> Maybe a

  destroyFromList :: [a] -> [a]
  destroyFromList = mapMaybe destroy

-------------------------------------------------

-- * Helper functions

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

-- * Instances

-------------------------------------------------

instance Updateable Airplane where
  move :: Airplane -> Airplane
  -- move airplane@Airplane {airplanePos = p, airplaneVelocity = v} = airplane {airplanePos = updatePosition p v}
  move airplane@Airplane {airplanePos = p, airplaneVelocity = v, airplaneType = t, airplaneHealth = h} = airplane {airplanePos = updatedPosition, airplaneVelocity = updatedVelocity, airplaneHealth = updatedHealth}
    where
      pos@(x, y) = updatePosition p v

      updatedVelocity = updateVelocity v
      -- \| t == Player1 || t == Player2 = updateVelocity v
      -- \| otherwise = v

      updatedPosition
        | t == Player1 || t == Player2 = (max C.screenMinX (min x C.screenMaxX), max C.screenMinY (min y C.screenMaxY))
        | otherwise = pos

      updatedHealth
        | t == Player1 || t == Player2 = h
        | otherwise = if x < C.screenMinX then 0 else h

  destroy :: Airplane -> Maybe Airplane
  destroy airplane@Airplane {airplaneHealth = h}
    | h <= 0 = Nothing
    | otherwise = Just airplane

instance Updateable Projectile where
  move :: Projectile -> Projectile
  move projectile@Projectile {projectilePos = p, projectileVelocity = v, projectileHealth = h} = projectile {projectilePos = updatedPos, projectileHealth = updatedHealth}
    where
      updatedPos@(x, y) = updatePosition p v
      updatedHealth
        | x < C.screenMinX || x > C.screenMaxX || y < C.screenMinY || y > C.screenMaxY = 0
        | otherwise = h

  destroy :: Projectile -> Maybe Projectile
  destroy projectile@Projectile {projectileHealth = h}
    | h <= 0 = Nothing
    | otherwise = Just projectile

instance Updateable PowerUp where
  move :: PowerUp -> PowerUp
  move powerUp = powerUp -- A powerUp is stationary in the current version

  destroy :: PowerUp -> Maybe PowerUp
  destroy pu@PowerUp {powerUpState = state, timeUntilDespawn = despawnTime, powerUpDuration = duration} = case state of
    PickedUp
      | duration <= 0 -> Nothing
      | otherwise -> Just pu
    WorldSpace
      | despawnTime <= 0 -> Nothing
      | otherwise -> Just pu

instance Updateable Particle where
  move :: Particle -> Particle
  move particle = particle -- A Particle is stationary in the current version

  destroy :: Particle -> Maybe Particle
  destroy p@Particle {particleSprites = sprites}
    | null sprites = Nothing
    | otherwise = Just p
