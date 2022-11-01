{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "[Replace {rtype = Expr, pos = SrcSpan {startLine = 25, startCol = 9, endLine = 27, endCol = 36}, subts = [("a",SrcSpan {startLine = 25, startCol = 14, endLine = 25, endCol = 30}),("f",SrcSpan {startLine = 27, startCol = 20, endLine = 27, endCol = 36}),("t",SrcSpan {startLine = 26, startCol = 19, endLine = 26, endCol = 86})], orig = "if a then (t) else f"}]" #-}

-- | This module defines the Collidable type class
module Collidable where

import Graphics.Gloss
import Model

-- TODO Naming refactor

-------------------------------------------------
-- Collidable class
-------------------------------------------------

class Collidable a b where
  collides :: a -> b -> Bool

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
-- Helper functions
-------------------------------------------------

checkCollision :: (Point, Point) -> (Point, Point) -> Bool
-- TODO (a1X, a1Y) (a2X, a2Y) b1 b2
checkCollision (r1p1, r1p2) (r2p1, r2p2) = fst r1p1 < fst r2p2 && fst r1p2 > fst r2p1 && snd r1p1 > snd r2p2 && snd r1p2 < snd r2p1

toHitBox :: Position -> Size -> (Point, Point)
-- TODO bigger name for s & p
toHitBox posistion@(posX, posY) (sizeX, sizeY) = (posistion, (posX + sizeX, posY - sizeY))

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Collidable Airplane Airplane where
  collides :: Airplane -> Airplane -> Bool
  collides
    Airplane {airplaneType = type1, airplanePos = pos1, airplaneSize = size1}
    Airplane {airplaneType = type2, airplanePos = pos2, airplaneSize = size2}
      | type1 == Player1 && type2 == Player2 = False
      | type1 == Player2 && type2 == Player1 = False
      | type1 == Player1 || type1 == Player2 = checkCollision (toHitBox pos1 size1) (toHitBox pos2 size2)
      | otherwise = False

instance Collidable Projectile Airplane where
  collides :: Projectile -> Airplane -> Bool
  collides
    Projectile {projectileOrigin = origin, projectilePos = _projectilePos, projectileSize = _projectileSize}
    Airplane {airplaneType = _type, airplanePos = _airplanePos, airplaneSize = _airplaneSize}
      | origin == Players && (_type == Player1 || _type == Player2) = False
      | origin == Enemies && _type /= Player1 && _type /= Player2 = False
      | otherwise = checkCollision (toHitBox _projectilePos _projectileSize) (toHitBox _airplanePos _airplaneSize)

-- TODO refactor bottom two
instance Collidable Projectile Projectile where
  collides :: Projectile -> Projectile -> Bool
  collides
    Projectile {projectileOrigin = o1, projectilePos = pos1, projectileSize = size1}
    Projectile {projectileOrigin = o2, projectilePos = pos2, projectileSize = size2}
      | o1 /= o2 = checkCollision (toHitBox pos1 size1) (toHitBox pos2 size2)
      | otherwise = False

instance Collidable Airplane PowerUp where
  collides :: Airplane -> PowerUp -> Bool
  collides
    Airplane {airplaneType = apType, airplanePos = apPosition, airplaneSize = apSize}
    PowerUp {powerUpState = puState, powerUpPos = puPosition, powerUpSize = puSize} =
      case puState of
        PickedUp -> False
        WorldSpace
          | apType == Player1 || apType == Player2 -> checkCollision (toHitBox apPosition apSize) (toHitBox puPosition puSize)
          | otherwise -> False
