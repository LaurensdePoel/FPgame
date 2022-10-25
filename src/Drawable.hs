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

-- Draw a rectangle on correct X,Y based on two Points and
instance Drawable ScreenBox where
  draw (ScreenBox ((pos1X, pos1Y), (pos2X, pos2Y))) =
    translate pos1X pos1Y $ -- translate image to first point (top left)
      fixOrigin $ -- fix the offset of the image created by Gloss when placed on the origin
        color (greyN 0.5) $
          rectangleWire boxSizeX boxSizeY -- Creates the image with the correct size
    where
      boxSizeX = absDiff pos1X pos2X -- calculates x and doesn't care about positive or negative points
      boxSizeY = absDiff pos1Y pos2Y -- calculates y and doesn't care about positive or negative points
      fixOrigin = translate (boxSizeX / 2) (- boxSizeY / 2)

-- Used to calculate the  absolute difference between two points
absDiff :: Num a => a -> a -> a
absDiff n = abs . (n -)