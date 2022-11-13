module Config where

-- TODO Add all values

import Assets
import Graphics.Gloss
import Helper
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

menuTextCharacterOffset :: Float
menuTextCharacterOffset = 60

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

itemParticleOffset :: Position
itemParticleOffset = (0, 10)

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
powerUpSpawnOdds = 500

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
      particleInterval = 80,
      particleTimer = 80,
      particleSprites = [errorSprite "Sprites not initialized"]
    }

-- * Background

backgroundAnimationSpeed :: Float
backgroundAnimationSpeed = 1.0

-- * Text

healthTextOffset :: (Float, Float)
healthTextOffset = (5, -48)

-- * Time

resetTime, timeInterval :: Time
resetTime = 0.0
timeInterval = 1.0

numRandomPoints :: Int
numRandomPoints = 10

playerSpriteRotation, enemySpriteRotation :: Float
playerSpriteRotation = 90
enemySpriteRotation = -90

player1SpawnLocation, player2SpawnLocation :: Position
player1SpawnLocation = (-300, 100)
player2SpawnLocation = (-300, -100)

-- * define airplanes

baseAirplane :: AirPlaneType -> Position -> Airplane
baseAirplane type' position =
  Airplane
    { airplaneType = type',
      airplanePos = position,
      airplaneDestinationPos = (screenMaxX, snd position),
      airplaneSize = airplaneSizeVar,
      airplaneVelocity = (0, 0),
      timeLastShot = 0.0,
      airplanePowerUps = [],
      airplaneSprite = errorSprite "default airplane config"
    }

fighterAirplane :: AirPlaneType -> Position -> Assets -> Picture -> Enemy
fighterAirplane type' position assets sprite =
  (baseAirplane type' position)
    { airplaneMaxVelocity = (-3, 3),
      airplaneHealth = 20,
      fireRate = Single 100.0,
      airplaneGun = enemyGun assets,
      airplaneSprite = sprite
    }

flybyAirplane :: AirPlaneType -> Position -> Assets -> Picture -> Enemy
flybyAirplane type' position assets sprite =
  (baseAirplane type' position)
    { airplaneMaxVelocity = (-4, 4),
      airplaneHealth = 20,
      fireRate = Burst 30.0,
      airplaneGun = enemyGun assets,
      airplaneSprite = sprite
    }

kamikazeAirplane :: AirPlaneType -> Position -> Assets -> Picture -> Enemy
kamikazeAirplane type' position assets sprite =
  (baseAirplane type' position)
    { airplaneMaxVelocity = (-5, 5),
      airplaneHealth = 40,
      fireRate = Single 0.0,
      airplaneGun = None,
      airplaneSprite = sprite
    }

playerAirplane :: AirPlaneType -> Position -> Assets -> Picture -> Enemy
playerAirplane type' position assets sprite =
  (baseAirplane type' position)
    { airplaneMaxVelocity = (-6, 6),
      airplaneHealth = 100,
      fireRate = Single 30.0,
      airplaneGun = playerGun assets,
      airplaneSprite = sprite
    }

-- * define airplane guns

playerGun :: Assets -> AirplaneGun
playerGun assets =
  AirplaneGun
    Projectile
      { projectileType = DoubleGun,
        projectilePos = (0, 0),
        projectileSize = projectileSizeVar,
        projectileVelocity = (16, 0),
        projectileHealth = 1,
        projectileDamage = 5,
        projectileOrigin = Players,
        projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate playerSpriteRotation $ getTexture "double-bullet" assets
      }

enemyGun :: Assets -> AirplaneGun
enemyGun assets =
  AirplaneGun
    Projectile
      { projectileType = Gun,
        projectilePos = (0, 0),
        projectileSize = projectileSizeVar,
        projectileVelocity = (-10, 0),
        projectileHealth = 1,
        projectileDamage = 10,
        projectileOrigin = Enemies,
        projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate enemySpriteRotation $ getTexture "bullet" assets
      }
