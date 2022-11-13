{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Updateable type class
module Updateable where

import Config as C
import Data.Maybe
import Helper (minMax)
import Model

-------------------------------------------------

-- * Updateable class

-------------------------------------------------

class Updateable a where
  -- | Updates the position of the Updateable
  move :: a -> a

  -- | Only returns the Updateable if the destroy condition aren't met
  destroy :: a -> Maybe a

  -- | Returns a list of Updateables where the conditions of destruction aren't met
  destroyFromList :: [a] -> [a]
  destroyFromList = mapMaybe destroy

  -- | Returns the center position of an Updateable
  getCenterPosition :: a -> Position

  -- | Gets the position of the closets Updateable b in [b] of Updateable a
  getClosestPosition :: Updateable b => a -> [b] -> Position
  getClosestPosition a [] = getCenterPosition a
  getClosestPosition a (b : bs) = foldr updateClosestB (getCenterPosition b) bs
    where
      currentPos :: Position
      currentPos = getCenterPosition a

      updateClosestB :: Updateable b => b -> Position -> Position
      updateClosestB b' rest =
        case isCloser position rest of
          True -> position
          False -> rest
        where
          isCloser :: Position -> Position -> Bool
          isCloser newPos pos = distance newPos < distance pos

          distance :: Position -> Float
          distance (x, y) = sqrt (((x - fst currentPos) ** 2) + ((y - snd currentPos) ** 2))

          position :: Position
          position = getCenterPosition b'

-------------------------------------------------

-- * Helper functions

-------------------------------------------------

-- | Updates position based on the velocity
updatePosition :: Position -> Velocity -> Position
updatePosition (posX, posY) (velocityX, velocityY) = (posX + velocityX, posY + velocityY)

-- | Updates the velocity
updateVelocity :: Velocity -> Velocity
updateVelocity (x, y) = (updateDirection x, updateDirection y)
  where
    updateDirection :: Float -> Float
    updateDirection value
      | isMovingDirectionPositive = max 0.0 (value - C.velocityReduction)
      | otherwise = min 0.0 (value + C.velocityReduction)
      where
        isMovingDirectionPositive = signum value == 1

-- | Calculates the center position
centerPosition :: Position -> Size -> Position
centerPosition (posX, posY) (sizeX, sizeY) = (posX + (sizeX * 0.5), posY - (sizeY * 0.5))

-------------------------------------------------

-- * Instances

-------------------------------------------------

instance Updateable Airplane where
  move :: Airplane -> Airplane
  -- \| Updates the position of the airplane
  move airplane@Airplane {airplanePos = _pos, airplaneSize = (_sizeX, _sizeY), airplaneVelocity = _velocity, airplaneType = _type, airplaneHealth = _health} =
    airplane {airplanePos = updatedPosition, airplaneVelocity = updatedVelocity, airplaneHealth = updatedHealth}
    where
      pos :: Position
      pos@(x, y) = updatePosition _pos _velocity

      updatedVelocity :: Velocity
      updatedVelocity = updateVelocity _velocity

      updatedPosition :: Position
      updatedPosition
        | _type == Player1 || _type == Player2 = (minMax (C.screenMinX, C.screenMaxX - _sizeX) x, minMax (C.screenMinY + _sizeY, C.screenMaxY) y)
        | otherwise = pos

      updatedHealth :: Int
      updatedHealth
        | _type == Player1 || _type == Player2 = _health
        | otherwise = if x < C.screenMinX then 0 else _health

  -- \| Only returns the airplane if the health is not zero
  destroy :: Airplane -> Maybe Airplane
  destroy airplane@Airplane {airplaneHealth = _health}
    | _health <= 0 = Nothing
    | otherwise = Just airplane

  -- \| Get the center position of an airplane
  getCenterPosition :: Airplane -> Position
  getCenterPosition Airplane {airplanePos = _pos, airplaneSize = _size} = centerPosition _pos _size

instance Updateable Projectile where
  -- \| Updates the position of the projectile
  move :: Projectile -> Projectile
  move projectile@Projectile {projectilePos = _pos, projectileSize = (_sizeX, _), projectileVelocity = _velocity, projectileHealth = _health} =
    projectile {projectilePos = updatedPos, projectileHealth = updatedHealth}
    where
      updatedPos@(x, y) = updatePosition _pos _velocity
      updatedHealth
        | x < C.screenMinX - _sizeX || x > C.screenMaxX + _sizeX || y < C.screenMinY || y > C.screenMaxY = 0
        | otherwise = _health

  -- \| Only returns the projectile if the health is not zero
  destroy :: Projectile -> Maybe Projectile
  destroy projectile@Projectile {projectileHealth = _health}
    | _health <= 0 = Nothing
    | otherwise = Just projectile

  -- \| Get the center position of a projectile
  getCenterPosition :: Projectile -> Position
  getCenterPosition Projectile {projectilePos = _pos, projectileSize = _size} = centerPosition _pos _size

instance Updateable PowerUp where
  -- \| Updates the position of the powerUp (A powerUp is stationary in the current version)
  move :: PowerUp -> PowerUp
  move powerUp = powerUp

  -- \| Only returns the powerUp if the timer is not zero
  destroy :: PowerUp -> Maybe PowerUp
  destroy powerUp@PowerUp {powerUpState = _state, timeUntilDespawn = _despawnTime, powerUpDuration = _duration} =
    case _state of
      PickedUp
        | _duration <= 0 -> Nothing
        | otherwise -> Just powerUp
      WorldSpace
        | _despawnTime <= 0 -> Nothing
        | otherwise -> Just powerUp

  -- \| Get the center position of a powerUp
  getCenterPosition :: PowerUp -> Position
  getCenterPosition PowerUp {powerUpPos = _pos, powerUpSize = _size} = centerPosition _pos _size

instance Updateable Particle where
  -- \| Updates the position of the particle (A particle is stationary in the current version)
  move :: Particle -> Particle
  move particle = particle

  -- \| Only returns the Particle if the animation isn't finished
  destroy :: Particle -> Maybe Particle
  destroy particle@Particle {particleSprites = _sprites}
    | null _sprites = Nothing
    | otherwise = Just particle

  -- \| Get the center position of a particle
  getCenterPosition :: Particle -> Position
  getCenterPosition Particle {particlePos = _pos, particleSize = _size} = centerPosition _pos _size
