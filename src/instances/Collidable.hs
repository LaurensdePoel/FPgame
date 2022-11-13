{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module defines the Collidable type class
module Collidable where

import Graphics.Gloss (Point)
import Model

-------------------------------------------------

-- * Collidable class

-------------------------------------------------

class Collidable a b where
  -- | Checks collision between two Collidables
  collides :: a -> b -> Bool

  -- | Update Collidables with the given function, when they collide
  applyOnCollisions :: Collidable a b => (a -> b -> (a, b)) -> [a] -> [b] -> ([a], [b])
  applyOnCollisions _ [] bs = ([], bs)
  applyOnCollisions _ as [] = (as, [])
  applyOnCollisions f (a : as) bs = let (_a, _bs) = applyOnCollisions f as bs in let (__as, __bs) = applyOnCollision f a _bs in (__as : _a, __bs)
    where
      applyOnCollision :: Collidable a b => (a -> b -> (a, b)) -> a -> [b] -> (a, [b])
      applyOnCollision _ a2 [] = (a2, [])
      applyOnCollision f2 a2 (b2 : bs2) =
        case a2 `collides` b2 of
          True -> let (updatedA, updatedB) = f2 _a2 b2 in (updatedA, updatedB : _bs2)
          False -> (_a2, b2 : _bs2)
        where
          (_a2, _bs2) = applyOnCollision f2 a2 bs2

-------------------------------------------------

-- * Helper functions

-------------------------------------------------

-- | Checks if two rectangles collide
checkCollision :: (Point, Point) -> (Point, Point) -> Bool
checkCollision ((a1X, a1Y), (a2X, a2Y)) ((b1X, b1Y), (b2X, b2Y)) = a1X < b2X && a2X > b1X && a1Y > b2Y && a2Y < b1Y

-- | Converts a position and size to a rectangle
toHitBox :: Position -> Size -> (Point, Point)
toHitBox position@(posX, posY) (sizeX, sizeY) = (position, (posX + sizeX, posY - sizeY))

-------------------------------------------------

-- * Instances

-------------------------------------------------

instance Collidable Airplane Airplane where
  -- \| Checks collision between two airplanes
  collides :: Airplane -> Airplane -> Bool
  collides
    Airplane {airplaneType = _type1, airplanePos = _pos1, airplaneSize = _size1}
    Airplane {airplaneType = _type2, airplanePos = _pos2, airplaneSize = _size2}
      | _type1 == Player1 && _type2 == Player2 = False
      | _type1 == Player2 && _type2 == Player1 = False
      | _type1 == Player1 || _type1 == Player2 = toHitBox _pos1 _size1 `checkCollision` toHitBox _pos2 _size2
      | otherwise = False

instance Collidable Projectile Airplane where
  -- \| Checks collision between a projectile and airplane
  collides :: Projectile -> Airplane -> Bool
  collides
    Projectile {projectileOrigin = _origin, projectilePos = _projectilePos, projectileSize = _projectileSize}
    Airplane {airplaneType = _type, airplanePos = _airplanePos, airplaneSize = _airplaneSize}
      | _origin == Players && (_type == Player1 || _type == Player2) = False
      | _origin == Enemies && _type /= Player1 && _type /= Player2 = False
      | otherwise = toHitBox _projectilePos _projectileSize `checkCollision` toHitBox _airplanePos _airplaneSize

instance Collidable Airplane Projectile where
  -- \| Checks collision between an airplane and projectile
  collides :: Airplane -> Projectile -> Bool
  collides = flip collides

instance Collidable Projectile Projectile where
  -- \| Checks collision between two projectiles
  collides :: Projectile -> Projectile -> Bool
  collides
    Projectile {projectileOrigin = _origin1, projectilePos = _pos1, projectileSize = _size1}
    Projectile {projectileOrigin = _origin2, projectilePos = _pos2, projectileSize = _size2}
      | _origin1 /= _origin2 = toHitBox _pos1 _size1 `checkCollision` toHitBox _pos2 _size2
      | otherwise = False

instance Collidable Airplane PowerUp where
  -- \| Checks collision between an airplane and powerUp
  collides :: Airplane -> PowerUp -> Bool
  collides
    Airplane {airplaneType = _airplaneType, airplanePos = _airplanePos, airplaneSize = _airplaneSize}
    PowerUp {powerUpState = _powerUpState, powerUpPos = _powerUpPos, powerUpSize = _powerUpSize} =
      case _powerUpState of
        PickedUp -> False
        WorldSpace
          | _airplaneType == Player1 || _airplaneType == Player2 -> toHitBox _airplanePos _airplaneSize `checkCollision` toHitBox _powerUpPos _powerUpSize
          | otherwise -> False