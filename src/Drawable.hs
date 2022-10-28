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
  draw Airplane {airplanePos = pos, airplaneSprite = sprite} = uncurry translate pos sprite

instance Drawable Projectile where
  draw :: Projectile -> Picture
  draw Projectile {projectilePos = pos, projectileSprite = sprite} = uncurry translate pos sprite

instance Drawable PowerUp where
  draw :: PowerUp -> Picture
  draw PowerUp {powerUpPos = pos, powerUpSprite = sprite} = uncurry translate pos sprite

instance Drawable Menu where
  draw :: Menu -> Picture
  draw Menu {fields = _fields} =
    pictures $
      Scale 1.3 1.3 (draw (head _fields)) : map draw (tail _fields)
  draw NoMenu = Blank -- tmp

instance Drawable Field where
  draw :: Field -> Picture
  draw Field {fieldName = _fieldName, fieldPosition = _fieldPosition} = Scale 0.25 0.25 $ translate `uncurry` _fieldPosition $ color white (Text _fieldName)