module Config where

-- TODO Add all values

import Assets
import Graphics.Gloss
import Model

-- * Window

fps :: Int
fps = 60

offset :: Int
offset = 0

-- ** Screen size

screenWidth :: Int
screenWidth = 1024

screenHeight :: Int
screenHeight = 768

screenMaxX :: Float
screenMaxX = fromIntegral screenWidth * 0.5

screenMinX :: Float
screenMinX = fromIntegral screenWidth * (-0.5)

screenMaxY :: Float
screenMaxY = fromIntegral screenHeight * 0.5

screenMinY :: Float
screenMinY = fromIntegral screenHeight * (-0.5)

-- * Menu

menuTextOffset :: Float
menuTextOffset = -200

menuTextStartHeight :: Float
menuTextStartHeight = 300

menuHeaderStartHeight :: Float
menuHeaderStartHeight = 300

-- * Sprite sizes and offsets

projectileSizeXY, airplaneSizeXY :: Float
projectileSizeXY = 16.0
airplaneSizeXY = 32.0

projectileSizeVar, airplaneSizeVar :: Size
projectileSizeVar = (projectileSizeXY, projectileSizeXY)
airplaneSizeVar = (airplaneSizeXY, airplaneSizeXY)

-- * Airplane base values

enemyXBounds, enemyYBounds :: (Float, Float)
enemyXBounds = (0.0, screenMaxX - airplaneSizeXY)
enemyYBounds = (screenMinY + airplaneSizeXY, screenMaxY - airplaneSizeXY)

-- ** Player base values

velocityStep :: Float
velocityStep = 0.6

-- ** Enemey base values

behaviourVelocitySteps :: Float
behaviourVelocitySteps = 0.25

-- * Multipliers

velocityReduction :: Float
velocityReduction = 0.2

destinationErrorMargin :: (Float, Float)
destinationErrorMargin = (0.2, 0.2)

-- * Projectile values

damageMultiplier :: Int
damageMultiplier = 2

-- * Power Ups

healthPackValue :: Int
healthPackValue = 30

powerPackValue :: Float
powerPackValue = 0.125

powerUpDefaultSize :: (Float, Float)
powerUpDefaultSize = (10, 10)

powerUpDespawnTime :: Float
powerUpDespawnTime = 350.0

powerUpDurationTime :: Float
powerUpDurationTime = 300.0

powerUpAnimationInterval :: Float
powerUpAnimationInterval = 10.0

powerUpSpawnOdds :: Int
powerUpSpawnOdds = 1000

-- * Sprites

spritesDefaultInterval :: Float
spritesDefaultInterval = 10.0

spritesDefault :: Sprites
spritesDefault =
  Sprites
    { spritesState = Idle,
      spritePos = (0, 0),
      spritesInterval = spritesDefaultInterval,
      spritesTimer = spritesDefaultInterval,
      idleSprites = [errorSprite "Idle sprites not set"],
      movingSprites = [errorSprite "Moving sprites not set"]
    }
-- * defaults

defaultTextParticle :: Particle
defaultTextParticle =
  Particle
    { particlePos = (0, 0),
      particleSize = (0, 0),
      particleInterval = 30,
      particleTimer = 30,
      particleSprites = [errorSprite "Sprites not initialized"]
    }
