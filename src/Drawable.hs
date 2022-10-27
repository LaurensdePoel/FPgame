{-# LANGUAGE InstanceSigs #-}

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
  draw :: Airplane -> Picture
  draw Airplane {airplanePos = p, airplaneSprite = s} = uncurry translate p s

instance Drawable Projectile where
  draw :: Projectile -> Picture
  draw Projectile {projectilePos = p, projectileSprite = s} = uncurry translate p s

instance Drawable Menu where
  draw :: Menu -> Picture
  draw Menu {fields = _fields} =
    pictures $
      Scale 1.3 1.3 (draw (head _fields)) : map draw (tail _fields)
  draw NoMenu = Blank -- tmp

--where
--  drawSelected Field {fieldName = _name, fieldPosition = _position} = translate `uncurry` _position $ Scale 0.25 0.25 (color blue $ Text _name)

instance Drawable Field where
  draw :: Field -> Picture
  draw Field {fieldName = _fieldName, fieldPosition = _fieldPosition} = Scale 0.25 0.25 $ translate `uncurry` _fieldPosition $ color blue (Text _fieldName)