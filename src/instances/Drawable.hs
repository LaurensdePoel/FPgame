{-# LANGUAGE InstanceSigs #-}

-- | This module defines the drawable type class
module Drawable where

import Graphics.Gloss
import Model

-------------------------------------------------

-- * Drawable class

-------------------------------------------------

class Drawable a where
  -- | Converts a Drawable into a Picture
  draw :: a -> Picture

  -- | Converts a list of Drawables into a list of pictures
  mapDraw :: [a] -> [Picture]
  mapDraw = map draw

-------------------------------------------------

-- * Instances

-------------------------------------------------

instance Drawable Airplane where
  -- \| Converts an airplane into a picture
  draw :: Airplane -> Picture
  draw Airplane {airplaneType = _type, airplanePos = _position, airplaneVelocity = _velocity, airplaneSprite = _sprite} =
    case _type of
      Kamikaze -> translate `uncurry` _position $ rotate (atan2 `uncurry` _velocity / pi * 180) _sprite
      _ -> translate `uncurry` _position $ _sprite

instance Drawable Projectile where
  -- \| Converts a projectile into a picture
  draw :: Projectile -> Picture
  draw Projectile {projectilePos = _position, projectileSprite = _sprite} = translate `uncurry` _position $ _sprite

instance Drawable PowerUp where
  -- \| Converts a powerUp into a picture
  draw :: PowerUp -> Picture
  draw PowerUp {powerUpSprites = _sprites, powerUpPos = _position} = draw _sprites {spritePos = _position}

instance Drawable Particle where
  -- \| Converts a particle into a picture
  draw :: Particle -> Picture
  draw Particle {particlePosition = _position, particleSprites = _sprites} = translate `uncurry` _position $ head _sprites

instance Drawable Sprites where
  -- \| Converts the first sprite of sprites into a picture
  draw :: Sprites -> Picture
  draw Sprites {spritesState = _state, spritePos = _position, idleSprites = _idleSprites, movingSprites = _movingSprites} =
    case _state of
      Idle -> uncurry translate _position $ head _idleSprites
      Moving -> uncurry translate _position $ head _movingSprites

instance Drawable Menu where
  -- \| Converts a menu into a picture
  draw :: Menu -> Picture
  draw Menu {fields = _fields} =
    pictures $
      Scale 1.3 1.3 (draw $ head _fields) : map draw (tail _fields)
  draw NoMenu = Blank
  draw NoMenuButFunction {} = Blank

instance Drawable Field where
  -- \| Converts a field into a picture
  draw :: Field -> Picture
  draw Field {fieldName = _fieldName, fieldPosition = _fieldPosition} = Scale 0.25 0.25 $ translate `uncurry` _fieldPosition $ color white (Text _fieldName)
