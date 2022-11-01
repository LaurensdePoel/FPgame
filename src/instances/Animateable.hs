{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Animateable type class
module Animateable where

import Model
import Graphics.Gloss (Picture)

---------------------------------------------------
-- Animateable class
-------------------------------------------------

class Animateable a where
  nextSprite :: a -> a

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Animateable Particle where
  nextSprite :: Particle -> Particle
  nextSprite particle@Particle {particleSprites = _sprites, particleInterval = _interval} = 
    particle {particleSprites = tail _sprites, particleTimer = _interval}

instance Animateable Sprites where
  nextSprite :: Sprites -> Sprites
  nextSprite sprites@Sprites {spritesState = _state, spritesInterval = _interval, movingSprites = _movingSprites, idleSprites = _idleSprites} =
    case _state of
      Idle -> sprites {idleSprites = updateHead _idleSprites, spritesTimer = _interval}
      Moving -> sprites {movingSprites = updateHead _movingSprites, spritesTimer = _interval}
    where
      updateHead :: [Picture] -> [Picture]
      updateHead [] = []
      updateHead (x : xs) = xs ++ [x]