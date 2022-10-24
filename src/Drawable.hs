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