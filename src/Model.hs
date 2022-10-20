{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import qualified Data.Map.Strict as M
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

newtype Size = Size Point

newtype Velocity = Velocity Point

data ProjectileType = None | Gun | DoubleGun | Rocket

newtype Damage = Damage Int

data Origin = Players | Enemies deriving (Eq)

class Collidable a b where
  collides :: a -> b -> Bool

data Airplane = Airplane
  { airplaneType :: AirPlaneType,
    airplanePos :: Position,
    airplaneSize :: Size,
    airplaneVelocity :: Velocity,
    airplaneHealth :: Int,
    -- fireRate :: FireRate
    -- timeLastShot :: Time
    -- AirplaneProjectile :: Projectile
    airplaneSprite :: Picture
  }

data AirPlaneType = Player | Fighter | Kamikaze

data Projectile = Projectile
  { projectileType :: ProjectileType,
    projectilePos :: Position,
    projectileSize :: Size,
    projectileVelocity :: Velocity,
    damage :: Damage,
    origin :: Origin,
    sprite :: Picture
  }

data GameState = Game
  { elapsedTime :: Float,
    status :: Status,
    tmpInt :: Int,
    players :: Airplane
    --enemies :: [Airplane]
    --level :: Level,
    --projectiles :: [Projectile],
    --players :: [Player],
    --powerUP :: [PowerUp]
  }

-- deriving (Show)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

initialState :: [Picture] -> GameState
initialState assetlist =
  Game
    { elapsedTime = 0,
      status = InGame,
      players =
        Airplane
          { airplaneType = Player,
            airplanePos = (-400, 0),
            airplaneSize = Size (50, 50),
            airplaneVelocity = Velocity (5, 5),
            airplaneHealth = 100,
            airplaneSprite = rotate 90 (head assetlist)
          },
      tmpInt = 0
    }

-- checkCollision :: (Point, Point) -> (Point, Point) -> Bool
-- checkCollision (r1p1, r1p2) (r2p1, r2p2) = fst (r1p1) < fst (r2p2) && fst (r1p2) > fst (r2p1) && snd (r1p1) > snd (r2p2) && snd (r1p2) < snd (r2p1)

-- toBoundingBox :: Position -> Size -> (Point, Point)
-- toBoundingBox (Position p@(pX, pY)) (Size (sX, sY)) = (p, (pX + sX, pY + sY))

-- airplaneToBoundingBox :: Airplane -> (Point, Point)
-- airplaneToBoundingBox Airplane {airplanePos = p, airplaneSize = s} = toBoundingBox p s

-- instance Collidable Player Enemy where
--   collides Player {playerAirplane = pPlane} Fighter {fighterPlane = ePlane} = checkCollision (airplaneToBoundingBox pPlane) (airplaneToBoundingBox ePlane)
--   collides Player {playerAirplane = pPlane} Kamikaze {kamikazePlane = ePlane} = checkCollision (airplaneToBoundingBox pPlane) (airplaneToBoundingBox ePlane)

-- instance Collidable Projectile Enemy where
--   collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Fighter {fighterPlane = fPlane}
--     | o /= Enemies = checkCollision (toBoundingBox pPos pSize) (airplaneToBoundingBox fPlane)
--     | otherwise = False
--   collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Kamikaze {kamikazePlane = kPlane}
--     | o /= Enemies = checkCollision (toBoundingBox pPos pSize) (airplaneToBoundingBox kPlane)
--     | otherwise = False

-- instance Collidable Projectile Player where
--   collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Player {playerAirplane = pPlane}
--     | o /= Players = checkCollision (toBoundingBox pPos pSize) (airplaneToBoundingBox pPlane)
--     | otherwise = False

-- instance Collidable Projectile Projectile where
