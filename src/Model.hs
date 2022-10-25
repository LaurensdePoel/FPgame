{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Map as Map
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Key)

data Status = InMenu | InGame

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

fps :: Int
fps = 60

newtype Size = Size Point

type Time = Float

type Velocity = Point

data ProjectileType = None | Gun | DoubleGun | Rocket

type Damage = Int

data Origin = Players | Enemies deriving (Eq)

-- firerate timelastshot
data FireRate = Single Time | Burst Time

newtype ScreenBox = ScreenBox (Point, Point)

data AirPlaneType = Player1 | Player2 | Fighter | Kamikaze deriving (Eq)

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
    -- powerUP :: [PowerUp],
    pressedKeys :: S.Set Key,
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
            { airplaneType = Player1,
              airplanePos = (-400, 0),
              airplaneSize = Size (50, 50),
              airplaneVelocity = (0, 0),
              airplaneHealth = 100,
              fireRate = Single 30.0,
              timeLastShot = 0.0,
              airplaneProjectile =
                Projectile
                  { projectileType = Gun,
                    projectilePos = (0, 0),
                    projectileSize = Size (30, 30),
                    projectileVelocity = (10, 0),
                    projectileHealth = 1,
                    projectileDamage = 30,
                    projectileOrigin = Players,
                    projectileSprite = rotate 90 $ getTexture "bullet" assetlist
                  },
              airplaneSprite = rotate 90 $ getTexture "player1" assetlist
            },
          Airplane
            { airplaneType = Player2,
              airplanePos = (-200, 0),
              airplaneSize = Size (100, 100),
              airplaneVelocity = (0, 0),
              airplaneHealth = 100,
              fireRate = Single 30.0,
              timeLastShot = 0.0,
              airplaneProjectile =
                Projectile
                  { projectileType = Gun,
                    projectilePos = (0, 0),
                    projectileSize = Size (1, 1),
                    projectileVelocity = (10, 0),
                    projectileHealth = 1,
                    projectileDamage = 30,
                    projectileOrigin = Players,
                    projectileSprite = rotate 90 $ getTexture "bullet" assetlist
                  },
              airplaneSprite = rotate 90 $ getTexture "player2" assetlist
            }
        ],
      enemies =
        [ -- tmp enemy
          Airplane
            { airplaneType = Fighter,
              airplanePos = (-10, -200),
              airplaneSize = Size (100, 100),
              airplaneVelocity = (0, 0),
              airplaneHealth = 100,
              fireRate = Burst 120.0,
              timeLastShot = 0.0,
              airplaneProjectile =
                Projectile
                  { projectileType = Gun,
                    projectilePos = (0, 0),
                    projectileSize = Size (30, 30),
                    projectileVelocity = (-10, 0),
                    projectileHealth = 1,
                    projectileDamage = 10,
                    projectileOrigin = Enemies,
                    projectileSprite = rotate (-90) $ getTexture "bullet" assetlist
                  },
              airplaneSprite = rotate (-90) $ getTexture "player1" assetlist
            }
        ],
      tmpInt = 0,
      pressedKeys = S.empty,
      projectiles = [],
      -- TODO add check that pos x1 < x2 || neg x1 > x2 and pos y1 > y2 || neg y1 < y2
      window = ScreenBox ((-400.0, 300.0), (400.0, -300.0))
    }

getTexture :: String -> Map String Picture -> Picture
getTexture s m = case Map.lookup s m of
  Nothing -> rotate (-90) $ Scale 0.25 0.25 (color red $ Text "error")
  Just x -> x
