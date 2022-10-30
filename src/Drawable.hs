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
  -- draw pu@PowerUp {powerUpAnimations = animations, powerUpPos = pos, powerUpSprite = sprite} =
  draw pu@PowerUp {powerUpSprites = sprites, powerUpPos = pos} = draw sprites {spritePos = pos}

-- case animationState animations of
--   Idle -> uncurry translate pos (head $ animationIdleSprites animations)
--   Moving -> uncurry translate pos (head $ animationMovingSprites animations)

instance Drawable Particle where
  draw :: Particle -> Picture
  draw Particle {particlePosition = pos, particleSprites = sprites} = uncurry translate pos $ head sprites

instance Drawable Sprites where
  draw :: Sprites -> Picture
  draw sprites@Sprites {spritesState = state, spritePos = pos} =
    case state of
      Idle -> uncurry translate pos (head $ idleSprites sprites)
      Moving -> uncurry translate pos (head $ movingSprites sprites)
