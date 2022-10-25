-- | This module defines the drawable type class
module Drawable where

import Graphics.Gloss
import Model

-------------------------------------------------
-- Drawable class
-------------------------------------------------

class Drawable a where
  draw :: a -> Picture

  mapDraw :: [a] -> [Picture]
  mapDraw = map draw

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Drawable Airplane where
  draw Airplane {airplanePos = p, airplaneSprite = s} = uncurry translate p s

instance Drawable Projectile where
  draw Projectile {projectilePos = p, projectileSprite = s} = uncurry translate p s

--TODO add translate to move to correct location -> Note that half the size is already applied
instance Drawable ScreenBox where
  draw (ScreenBox ((pos1X, pos1Y), (pos2X, pos2Y))) =
    --translate pos1X pos1Y $
    color wallColor $
      rectangleWire boxSizeX boxSizeY
    where
      boxSizeX = absDiff pos1X pos2X
      boxSizeY = absDiff pos1Y pos2Y
      wallColor = greyN 0.5

-- Used to calculate the  absolute difference between two points
absDiff :: Num a => a -> a -> a
absDiff n = abs . (n -)