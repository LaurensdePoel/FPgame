{-# LANGUAGE InstanceSigs #-}

-- | This module defines the drawable type class
module Drawable where

import Graphics.Gloss
import Model

-- TODO Naming refactor

-------------------------------------------------
-- Drawable class
-------------------------------------------------

class Drawable a where
  draw :: a -> Picture

  mapDraw :: [a] -> [Picture]
  mapDraw = map draw

-------------------------------------------------
-- Helper functions
-------------------------------------------------

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Drawable Airplane where
  draw :: Airplane -> Picture
  draw Airplane {airplaneType = _type, airplanePos = pos, airplaneVelocity = (x, y), airplaneSprite = sprite} =
    case _type of
      Kamikaze -> uncurry translate pos $ rotate (atan2 x y / pi * 180) sprite
      _ -> uncurry translate pos sprite

instance Drawable Projectile where
  draw :: Projectile -> Picture
  draw Projectile {projectilePos = pos, projectileSprite = sprite} = uncurry translate pos sprite

instance Drawable PowerUp where
  draw :: PowerUp -> Picture
  draw PowerUp {powerUpSprites = sprites, powerUpPos = pos} = draw sprites {spritePos = pos}

instance Drawable Particle where
  draw :: Particle -> Picture
  draw Particle {particlePosition = pos, particleSprites = sprites} = translate `uncurry` pos $ head sprites

instance Drawable Sprites where
  draw :: Sprites -> Picture
  draw sprites@Sprites {spritesState = state, spritePos = pos} =
    case state of
      Idle -> uncurry translate pos (head $ idleSprites sprites)
      Moving -> uncurry translate pos (head $ movingSprites sprites)

instance Drawable Menu where
  draw :: Menu -> Picture
  draw Menu {fields = _fields} =
    pictures $
      Scale 1.3 1.3 (draw (head _fields)) : map draw (tail _fields)
  draw NoMenu = Blank
  draw NoMenuButFunction {} = Blank

instance Drawable Field where
  draw :: Field -> Picture
  draw Field {fieldName = _fieldName, fieldPosition = _fieldPosition} = Scale 0.25 0.25 $ translate `uncurry` _fieldPosition $ color white (Text _fieldName)
