{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Animateable type class
module Animateable where

import Model

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
  nextSprite p@Particle {particleSprites = sprites, particleInterval = interval} = p {particleSprites = tail sprites, particleTimer = interval}