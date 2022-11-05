module Random where

import Assets
import Config as C
import Model
import System.Random

-- | Generates a random position
getRandomPoint :: RandomGen a => (Float, Float) -> (Float, Float) -> a -> Position
getRandomPoint rangeX rangeY gen = (x, y)
  where
    (x, gen') = randomR rangeX gen
    (y, _) = randomR rangeY gen'

-- TODO: these instances are ugly

-- | PowerUpTypes instance of enum -- TODO should be fun if the powerUp values where random, not sure if this could be handled here
instance Enum PowerUpTypes where
  toEnum 0 = HealthPack 100
  toEnum 1 = PowerPack 0.5

  fromEnum (HealthPack _) = 0
  fromEnum (PowerPack _) = 1

-- | PowerUpTypes instance of bound
instance Bounded PowerUpTypes where
  minBound = (HealthPack 0)
  maxBound = (PowerPack 0)

-- | PowerUpTypes instance of random
instance Random PowerUpTypes where
  -- random :: RandomGen a => a -> (PowerUpTypes, a)
  random gen = case randomR (fromEnum (minBound :: PowerUpTypes), fromEnum (maxBound :: PowerUpTypes)) gen of
    (result, gen') -> (toEnum result, gen')

  -- randomR :: RandomGen a => (PowerUpTypes, PowerUpTypes) -> a -> (PowerUpTypes, a)
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
            idleSprites = powerUpSprites,
            movingSprites = []
          }
    }
  where
    getRandomPowerUpType :: PowerUpTypes
    getRandomPowerUpType = fst $ random gen

    getRandomPos :: Position
    getRandomPos = getRandomPoint xRange yRange gen
      where
        xRange = (C.screenMinX, C.screenMaxX)
        yRange = (C.screenMinY, C.screenMaxY)

    powerUpSprites = case getRandomPowerUpType of
      (PowerPack _) -> [getTexture "power-pack_1" assetList, getTexture "power-pack_2" assetList]
      (HealthPack _) -> [getTexture "health-pack_1" assetList, getTexture "health-pack_2" assetList]

spawnPowerUp :: RandomGen a => a -> Assets -> Maybe PowerUp -- TODO: this function could be cleaner with monad or something
spawnPowerUp gen assets
  | probability < odds = Just $ getRandomPowerUp gen'' assets
  | otherwise = Nothing
  where
    -- \| Probability to spawn a power up
    (odds, gen') = randomR (0.0 :: Float, 0.001 :: Float) gen -- TODO: not working properly to many spawn
    (probability, gen'') = randomR (0.0 :: Float, 1.0 :: Float) gen'
