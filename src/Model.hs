{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Map as Map
import Graphics.Gloss

data Status = InMenu | InGame

type Position = Point

instance Num Position where
  (+) :: Position -> Position -> Position
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  (-) :: Position -> Position -> Position
  (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  (*) :: Position -> Position -> Position
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  signum :: Position -> Position
  signum (x, y) = (signum x, signum y)
  abs :: Position -> Position
  abs (x, y) = (abs x, abs y)
  negate :: Position -> Position
  negate (x, y) = (negate x, negate y)
  fromInteger :: Integer -> Position
  fromInteger x = (fromInteger x, fromInteger x)

fps :: Int
fps = 60

newtype Size = Size Point

type Time = Float

newtype Velocity = Velocity Point

data ProjectileType = None | Gun | DoubleGun | Rocket

newtype Damage = Damage Int

data Origin = Players | Enemies deriving (Eq)

-- firerate timelastshot
data FireRate = Single Time | Burst Time

newtype ScreenBox = ScreenBox (Point, Point)

data Airplane = Airplane
  { airplaneType :: AirPlaneType,
    airplanePos :: Position,
    airplaneSize :: Size,
    airplaneVelocity :: Velocity,
    airplaneHealth :: Int,
    fireRate :: FireRate,
    timeLastShot :: Time,
    airplaneProjectile :: Projectile,
    airplaneSprite :: Picture
  }

data AirPlaneType = Player | Fighter | Kamikaze deriving (Eq)

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

data GameState = Game
  { elapsedTime :: Float,
    status :: Status,
    tmpInt :: Int,
    players :: [Airplane],
    enemies :: [Airplane],
    -- level :: Level,
    projectiles :: [Projectile],
    -- players :: [Player],
    -- powerUP :: [PowerUp],
    window :: ScreenBox
  }

-- deriving (Show)

initialState :: Map String Picture -> GameState
initialState assetlist =
  Game
    { elapsedTime = 0,
      status = InGame,
      players =
        [ Airplane
            { airplaneType = Player,
              airplanePos = (-400, 0),
              airplaneSize = Size (50, 50),
              airplaneVelocity = Velocity (5, 5),
              airplaneHealth = 100,
              fireRate = Single 30.0,
              timeLastShot = 0.0,
              airplaneProjectile =
                Projectile
                  { projectileType = Gun,
                    projectilePos = (0, 0),
                    projectileSize = Size (30, 30),
                    projectileVelocity = Velocity (10, 0),
                    projectileHealth = 1,
                    projectileDamage = Damage 30,
                    projectileOrigin = Players,
                    projectileSprite = rotate 90 $ getTexture "bullet" assetlist
                  },
              airplaneSprite = rotate 90 $ getTexture "player1" assetlist
            }
        ],
      enemies =
        [ -- tmp enemy
          Airplane
            { airplaneType = Fighter,
              airplanePos = (-10, -200),
              airplaneSize = Size (50, 50),
              airplaneVelocity = Velocity (0, 0),
              airplaneHealth = 70,
              fireRate = Burst 120.0,
              timeLastShot = 0.0,
              airplaneProjectile =
                Projectile
                  { projectileType = Gun,
                    projectilePos = (0, 0),
                    projectileSize = Size (30, 30),
                    projectileVelocity = Velocity (-10, 0),
                    projectileHealth = 1,
                    projectileDamage = Damage 10,
                    projectileOrigin = Enemies,
                    projectileSprite = rotate (-90) $ getTexture "bullet" assetlist
                  },
              airplaneSprite = rotate (-90) $ getTexture "player1" assetlist
            }
        ],
      tmpInt = 0,
      projectiles = [],
      window = ScreenBox ((-1000.0, -1000.0), (1000.0, 1000.0))
    }

getTexture :: String -> Map String Picture -> Picture
getTexture s m = case Map.lookup s m of
  Nothing -> rotate (-90) $ Scale 0.25 0.25 (color red $ Text "error")
  Just x -> x
