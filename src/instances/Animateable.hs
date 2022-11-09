{-# LANGUAGE InstanceSigs #-}

-- | This module defines the Animateable type class
module Animateable where

import Config as C
import Graphics.Gloss
import Model

---------------------------------------------------

-- * Animateable class

-------------------------------------------------

class Animateable a where
  -- | Updates the sprite of an Animateable
  nextSprite :: a -> a

-------------------------------------------------

-- * Instances

-------------------------------------------------

instance Animateable Particle where
  nextSprite :: Particle -> Particle
  -- \| Removes the current sprite
  nextSprite particle@Particle {particleSprites = _sprites, particleInterval = _interval} =
    particle {particleSprites = tail _sprites, particleTimer = _interval}

instance Animateable Sprites where
  -- \| Moves the current sprite to the end of the sprite list
  nextSprite :: Sprites -> Sprites
  nextSprite sprites@Sprites {spritesState = _state, spritesInterval = _interval, movingSprites = _movingSprites, idleSprites = _idleSprites} =
    case _state of
      Idle -> sprites {idleSprites = updateHead _idleSprites, spritesTimer = _interval}
      Moving -> sprites {movingSprites = updateHead _movingSprites, spritesTimer = _interval}
    where
      updateHead :: [Picture] -> [Picture]
      updateHead [] = []
      updateHead (x : xs) = xs ++ [x]

instance Animateable Background where
  nextSprite :: Background -> Background
  nextSprite background@Background {backgroundPos = _pos, backgroundSprite = _sprite} = background {backgroundPos = newPos}
    where
      newPos :: Position
      newPos = updatePosition _pos

      updatePosition :: Position -> Position
      updatePosition (x, y)
        | x <= (C.screenMinX * 2) = (0, 0)
        | otherwise = (x - 1, y)
