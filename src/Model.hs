{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

-- TODO REORDER
-- TODO NAMECHANGE

import Data.Map as Map
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Key)

data Status = InMenu | InGame deriving (Eq)

type Position = Point

-- This is unnecessary
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

type Assets = (Map String Picture)

type Size = Point

type Time = Float

type Velocity = Point

data ProjectileType = Gun | DoubleGun | Rocket

type Damage = Int

data Origin = Players | Enemies deriving (Eq)

-- firerate timelastshot
data FireRate = Single Time | Burst Time

newtype ScreenBox = ScreenBox (Point, Point)

data AirPlaneType = Player1 | Player2 | Fighter | Kamikaze | FlyBy deriving (Eq)

data PowerUpTypes = HealthPack Int | PowerPack Float

data PowerUpState = WorldSpace | PickedUp

data AnimationState = Idle | Moving

data AirplaneGun = AirplaneGun Projectile | None

data Sprites = Sprites
  { spritesState :: AnimationState,
    spritePos :: Position,
    spritesInterval :: Time,
    spritesTimer :: Time,
    idleSprites :: [Picture],
    movingSprites :: [Picture]
  }

data Particle = Particle
  { particlePosition :: Position,
    particleSize :: Size,
    particleInterval :: Time,
    particleTimer :: Time,
    particleSprites :: [Picture]
  }

data PowerUp = PowerUp
  { powerUpPos :: Position,
    powerUpSize :: Size,
    powerUpType :: PowerUpTypes,
    powerUpState :: PowerUpState,
    timeUntilDespawn :: Time,
    powerUpDuration :: Time,
    -- powerUpSprite :: Picture,
    powerUpSprites :: Sprites
  }

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

type Enemy = Airplane

data Wave = Wave
  { enemiesInWave :: [Enemy],
    waveTimer :: Time
  }

data Level = Level
  { levelNr :: Int,
    waves :: [Wave]
  }

data Menu
  = Menu
      { -- header :: Picture,
        fields :: [Field],
        -- menuBackground :: Picture,
        returnMenu :: Menu
      }
  | NoMenu
  | NoMenuButFunction (GameState -> GameState)

data Field = Field
  { fieldName :: String,
    fieldPosition :: Position,
    subMenu :: Menu
  }

data GameState = GameState
  { elapsedTime :: Float,
    status :: Status,
    players :: [Airplane],
    enemies :: [Enemy],
    level :: Level,
    projectiles :: [Projectile],
    powerUps :: [PowerUp],
    particleMap :: Map String Particle,
    particles :: [Particle],
    pressedKeys :: S.Set Key,
    menu :: Menu,
    tmpassetList :: Assets
  }