{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module defines the Collidable type class
module Collidable where

import Graphics.Gloss
import Model

-------------------------------------------------
-- Collidable class
-------------------------------------------------

class Collidable a b where
  collides :: a -> b -> Bool

-------------------------------------------------
-- Helper functions
-------------------------------------------------

checkCollision :: (Point, Point) -> (Point, Point) -> Bool
checkCollision (r1p1, r1p2) (r2p1, r2p2) = fst (r1p1) < fst (r2p1) + fst (r2p2) && fst (r1p1) + fst (r1p2) > fst (r2p1) && snd (r1p1) < snd (r2p1) + snd (r2p2) && snd (r1p2) + snd (r1p1) > snd (r2p1)

toHitBox :: Position -> Size -> (Point, Point)
toHitBox p@(pX, pY) (Size (sX, sY)) = (p, (pX + sX, pY + sY))

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Collidable Airplane Airplane where
  collides
    Airplane {airplaneType = type1, airplanePos = pos1, airplaneSize = size1}
    Airplane {airplaneType = type2, airplanePos = pos2, airplaneSize = size2}
      | type1 == Player1 && type2 == Player2 = False
      | type1 == Player2 && type2 == Player1 = False
      | type1 == Player1 || type1 == Player2 = checkCollision (toHitBox pos1 size1) (toHitBox pos2 size2)
      | otherwise = False

instance Collidable Airplane ScreenBox where
  collides
    Airplane {airplaneType = t, airplanePos = pos@(px, _), airplaneSize = size}
    (ScreenBox screenBox@((x, _), _))
      | t == Player1 || t == Player2 = checkCollision (toHitBox pos size) screenBox
      | otherwise = px < x

instance Collidable Projectile Airplane where
  collides
    Projectile {projectileOrigin = o, projectilePos = pPos, projectileSize = pSize}
    Airplane {airplaneType = t, airplanePos = aPos, airplaneSize = aSize}
      | o == Players && (t == Player1 || t == Player2) = False
      | o == Enemies && t /= Player1 && t /= Player2 = False
      | otherwise = checkCollision (toHitBox pPos pSize) (toHitBox aPos aSize)

instance Collidable Projectile Projectile where
  collides
    Projectile {projectileOrigin = o1, projectilePos = pos1, projectileSize = size1}
    Projectile {projectileOrigin = o2, projectilePos = pos2, projectileSize = size2}
      | o1 /= o2 = checkCollision (toHitBox pos1 size1) (toHitBox pos2 size2)
      | otherwise = False

instance Collidable Projectile ScreenBox where
  collides
    Projectile {projectilePos = pos, projectileSize = size}
    (ScreenBox screenBox) =
      checkCollision (toHitBox pos size) screenBox
