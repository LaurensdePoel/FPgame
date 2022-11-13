{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Random where

import Assets (getParticle, getTexture)
import Config as C
import Graphics.Gloss (Picture)
import Item ()
import Model
import System.Random (Random (random, randomR), RandomGen)
import Updateable ()

-- | Generates a random position
getRandomPoint :: RandomGen a => (Float, Float) -> (Float, Float) -> a -> (Position, a)
getRandomPoint rangeX rangeY gen = ((x, y), gen'')
  where
    (x, gen') = randomR rangeX gen
    (y, gen'') = randomR rangeY gen'

getRandomPoints :: RandomGen a => (Float, Float) -> (Float, Float) -> Int -> a -> [Position]
getRandomPoints rangeX rangeY amount gen = snd $ foldr (\_ (gen', points) -> let (point, gen'') = getRandomPoint rangeX rangeY gen' in (gen'', point : points)) (gen, []) [0 .. amount]

-- | PowerUpTypes instance of random
instance Random PowerUpTypes where
  random :: RandomGen g => g -> (PowerUpTypes, g)
  random gen = case randomR (fromEnum (minBound :: PowerUpTypes), fromEnum (maxBound :: PowerUpTypes)) gen of
    (result, gen') -> (toEnum result, gen')

  randomR :: RandomGen g => (PowerUpTypes, PowerUpTypes) -> g -> (PowerUpTypes, g)
  randomR (a, b) gen = case randomR (fromEnum a, fromEnum b) gen of
    (result, gen') -> (toEnum result, gen')

-- | Generate a random powerUp
getRandomPowerUp :: RandomGen a => a -> Assets -> PowerUp
getRandomPowerUp gen assets =
  PowerUp
    { powerUpPos = getRandomPos,
      powerUpType = getRandomPowerUpType,
      powerUpSize = C.powerUpDefaultSize,
      powerUpState = WorldSpace,
      timeUntilDespawn = C.powerUpDespawnTime,
      powerUpDuration = C.powerUpDurationTime,
      powerUpSprites =
        C.spritesDefault
          { spritesInterval = C.powerUpAnimationInterval,
            spritesTimer = C.powerUpAnimationInterval,
            idleSprites = sprites
          }
    }
  where
    -- \| Get random powerUp type
    getRandomPowerUpType :: PowerUpTypes
    getRandomPowerUpType = fst $ random gen

    -- \| Get random position for the powerUp
    getRandomPos :: Position
    getRandomPos = fst $ getRandomPoint xRange yRange gen
      where
        xRange = (C.screenMinX, C.screenMaxX)
        yRange = (C.screenMinY, C.screenMaxY)

    -- \| Get the corresponding sprites
    sprites :: [Picture]
    sprites = case getRandomPowerUpType of
      (PowerPack _) -> [getTexture "power-pack_1" assets, getTexture "power-pack_2" assets]
      (HealthPack _) -> [getTexture "health-pack_1" assets, getTexture "health-pack_2" assets]

-- | Probability of creating a power up to spawn
spawnPowerUp :: RandomGen a => a -> Assets -> Maybe PowerUp
spawnPowerUp gen assets
  | probability < 1 = Just $ getRandomPowerUp gen' assets
  | otherwise = Nothing
  where
    -- \| Probability to spawn a power up
    (probability, gen') = randomR (0 :: Int, C.powerUpSpawnOdds :: Int) gen

-- | Probability to spawn a powerUp
randomPowerUps :: RandomGen a => GameState -> a -> GameState
randomPowerUps gs@GameState {powerUps = _powerUps, particles = _particles, particleMap = _particleMap, assetMap = _assets} gen =
  case spawnPowerUp gen _assets of
    Just x -> gs {powerUps = x : _powerUps, particles = powerUpParticle : _particles}
      where
        powerUpParticle :: Particle
        powerUpParticle = (getParticle "powerUpTimer" _particleMap) {particlePos = newParticlePos}

        newParticlePos :: Position
        newParticlePos = powerUpPos x + C.itemParticleOffset
    Nothing -> gs
