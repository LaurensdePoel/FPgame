{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Map as Map (Map)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Graphics.Gloss (Picture, Point)
import Graphics.Gloss.Interface.IO.Interact (Key)

-- * GameState

data Status = InMenu | InGame deriving (Eq)

type Time = Float

data IOActions = IOActions
  { releadLevels :: Bool,
    quitGame :: Bool
  }

data GameState = GameState
  { elapsedTime :: Float,
    status :: Status,
    nrOfPlayers :: Int,
    players :: [Airplane],
    enemies :: [Enemy],
    levels :: [Level],
    currentLevel :: Level,
    currentLevelNr :: Int,
    projectiles :: [Projectile],
    powerUps :: [PowerUp],
    particleMap :: Particles,
    particles :: [Particle],
    pressedKeys :: S.Set Graphics.Gloss.Interface.IO.Interact.Key,
    menu :: Menu,
    levelSelectMenu :: Menu,
    assetMap :: Assets,
    ioActions :: IOActions
  }

-- * Assets and Sprites

type Assets = (Map String Picture)

type Particles = (Map String Particle)

data AnimationState = Idle | Moving

data Sprites = Sprites
  { spritesState :: AnimationState,
    spritePos :: Position,
    spritesInterval :: Time,
    spritesTimer :: Time,
    idleSprites :: [Picture],
    movingSprites :: [Picture]
  }

data Particle = Particle
  { particlePos :: Position,
    particleSize :: Size,
    particleInterval :: Time,
    particleTimer :: Time,
    particleSprites :: [Picture]
  }

-- * Items

data PowerUpTypes = HealthPack Int | PowerPack Float

data PowerUpState = WorldSpace | PickedUp

data PowerUp = PowerUp
  { powerUpPos :: Position,
    powerUpSize :: Size,
    powerUpType :: PowerUpTypes,
    powerUpState :: PowerUpState,
    timeUntilDespawn :: Time,
    powerUpDuration :: Time,
    powerUpSprites :: Sprites
  }

-- * Airplanes

type Enemy = Airplane

data AirPlaneType = Player1 | Player2 | Fighter | Kamikaze | FlyBy deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AirplaneGun = AirplaneGun Projectile | None

instance Eq AirplaneGun where
  None == None = True
  (AirplaneGun _) == (AirplaneGun _) = True
  _ == _ = False

data Airplane = Airplane
  { airplaneType :: AirPlaneType,
    airplanePos :: Position,
    airplaneDestinationPos :: Position,
    airplaneSize :: Size,
    airplaneVelocity :: Velocity,
    airplaneMaxVelocity :: Velocity,
    airplaneHealth :: Int,
    fireRate :: FireRate,
    timeLastShot :: Time,
    airplaneGun :: AirplaneGun,
    airplaneSprite :: Picture,
    airplanePowerUps :: [PowerUp]
  }
  deriving (Generic)

-- ** Projectiles

type Damage = Int

data FireRate = Single Time | Burst Time

data ProjectileType = Gun | DoubleGun deriving (Eq)

data Origin = Players | Enemies deriving (Eq)

data Projectile = Projectile
  { projectileType :: ProjectileType,
    projectilePos :: Position,
    projectileSize :: Size,
    projectileVelocity :: Velocity,
    projectileHealth :: Int,
    projectileDamage :: Damage,
    projectileOrigin :: Origin,
    projectileSprite :: Picture
  }

-- * Levels

data Background = Background
  { backgroundPos :: Position,
    backgroundSprite :: Picture
  }

data Level = Level
  { levelNr :: Int,
    levelBackground :: Background,
    waves :: [Wave]
  }

data Wave = Wave
  { enemiesInWave :: [Enemy],
    waveTimer :: Time
  }

-- * Interface

data Menu
  = Menu
      { header :: String,
        fields :: [Field],
        menuBackground :: Picture,
        returnMenu :: Menu
      }
  | NoMenu
  | NoMenuButFunction (GameState -> GameState)

data Field = Field
  { fieldName :: String,
    fieldPosition :: Position,
    subMenu :: Menu
  }

-- * Global Parameters

type Position = Point

type Size = Point

type Velocity = Point

instance Num Point where
  (+) :: Point -> Point -> Point
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  (-) :: Point -> Point -> Point
  (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  (*) :: Point -> Point -> Point
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  signum :: Point -> Point
  signum (x, y) = (signum x, signum y)
  abs :: Point -> Point
  abs (x, y) = (abs x, abs y)
  negate :: Point -> Point
  negate (x, y) = (negate x, negate y)
  fromInteger :: Integer -> Point
  fromInteger x = (fromInteger x, fromInteger x)