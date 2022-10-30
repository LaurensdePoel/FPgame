{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Animateable type class
module Animateable where

import Model

---------------------------------------------------
-- Animateable class
-------------------------------------------------

class Animateable a where
  nextSprite :: a -> a
  updateAnimation :: a -> a

-------------------------------------------------
-- Instances
-------------------------------------------------

instance Animateable Particle where
  nextSprite :: Particle -> Particle
  nextSprite p@Particle {particleSprites = sprites, particleInterval = interval} = p {particleSprites = tail sprites, particleTimer = interval}

instance Animateable Sprites where
  nextSprite :: Sprites -> Sprites
  nextSprite sprites@Sprites {spritesState = state} =
    case state of
      Idle -> sprites {idleSprites = updateHead $ idleSprites sprites, spritesTimer = spritesInterval sprites}
      Moving -> sprites {movingSprites = updateHead $ movingSprites sprites, spritesTimer = spritesInterval sprites}
    where
      updateHead (x : xs) = xs ++ [x]