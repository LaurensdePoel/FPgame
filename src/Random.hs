{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Random where

import Assets
import Config as C
import Graphics.Gloss (Picture)
import Model
import System.Random

-- TODO: Move magic numbers to config file

-- | Generates a random position
getRandomPoint :: RandomGen a => (Float, Float) -> (Float, Float) -> a -> Position
getRandomPoint rangeX rangeY gen = (x, y)
  where
    (x, gen') = randomR rangeX gen
    (y, _) = randomR rangeY gen'

-- TODO: these instances are ugly

-- | PowerUpTypes instance of enum -- TODO should be fun if the powerUp values where random, not sure if this could be handled here
instance Enum PowerUpTypes where
  toEnum :: Int -> PowerUpTypes
  toEnum 0 = HealthPack 100
  toEnum _ = PowerPack 0.5

  fromEnum :: PowerUpTypes -> Int
  fromEnum (HealthPack _) = 0
  fromEnum (PowerPack _) = 1

-- | PowerUpTypes instance of bound
instance Bounded PowerUpTypes where
  minBound :: PowerUpTypes
  minBound = HealthPack 0
  maxBound :: PowerUpTypes
  maxBound = PowerPack 0

-- | PowerUpTypes instance of random
instance Random PowerUpTypes where
  -- random :: RandomGen a => a -> (PowerUpTypes, a)
  random :: RandomGen g => g -> (PowerUpTypes, g)
  random gen = case randomR (fromEnum (minBound :: PowerUpTypes), fromEnum (maxBound :: PowerUpTypes)) gen of
    (result, gen') -> (toEnum result, gen')

  -- randomR :: RandomGen a => (PowerUpTypes, PowerUpTypes) -> a -> (PowerUpTypes, a)
  randomR :: RandomGen g => (PowerUpTypes, PowerUpTypes) -> g -> (PowerUpTypes, g)
  randomR (a, b) gen = case randomR (fromEnum a, fromEnum b) gen of
    (result, gen') -> (toEnum result, gen')

-- | Generate a random powerUp --TODO: more parts of powerUp could be random and not sure if the instances should be defined in this file
getRandomPowerUp :: RandomGen a => a -> Assets -> PowerUp
getRandomPowerUp gen assetList =
  PowerUp
    { powerUpPos = getRandomPos,
      powerUpType = getRandomPowerUpType,
      powerUpSize = (10, 10),
      powerUpState = WorldSpace,
      timeUntilDespawn = 1000.0,
      powerUpDuration = 500.0,
      powerUpSprites =
        Sprites
          { spritesState = Idle,
            spritePos = (0, 0),
            spritesInterval = 10.0,
            spritesTimer = 10.0,
            idleSprites = sprites,
            movingSprites = []
          }
    }
  where
    -- \| Get random powerUp type
    getRandomPowerUpType :: PowerUpTypes
    getRandomPowerUpType = fst $ random gen

    -- \| Get random position for the powerUp
    getRandomPos :: Position
    getRandomPos = getRandomPoint xRange yRange gen
      where
        xRange = (C.screenMinX, C.screenMaxX)
        yRange = (C.screenMinY, C.screenMaxY)

    -- \| Get the corresponding sprites
    sprites :: [Picture]
    sprites = case getRandomPowerUpType of
      (PowerPack _) -> [getTexture "power-pack_1" assetList, getTexture "power-pack_2" assetList]
      (HealthPack _) -> [getTexture "health-pack_1" assetList, getTexture "health-pack_2" assetList]

-- | Probability of creating a power up to spawn
spawnPowerUp :: RandomGen a => a -> Assets -> Maybe PowerUp -- TODO: this function could be cleaner with monad or something
spawnPowerUp gen assets
  | probability < 1 = Just $ getRandomPowerUp gen' assets
  | otherwise = Nothing
  where
    -- \| Probability to spawn a power up
    (probability, gen') = randomR (0 :: Int, 1000 :: Int) gen
