{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

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

newtype Velocity = Velocity Point

data ProjectileType = None | Gun | DoubleGun | Rocket

newtype Damage = Damage Int

data Origin = Players | Enemies deriving (Eq)

class Collidable a b where
  collides :: a -> b -> Bool

class Updateable a where
  move :: a -> a
  shoot :: a -> a

  updateAll :: a -> a
  updateAll = shoot . move

class Drawable a where
  draw :: a -> Picture

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

data AirPlaneType = Player | Fighter | Kamikaze deriving (Eq)

data Projectile = Projectile
  { projectileType :: ProjectileType,
    projectilePos :: Position,
    projectileSize :: Size,
    projectileVelocity :: Velocity,
    projectileDamage :: Damage,
    projectileOrigin :: Origin,
    projectileSprite :: Picture
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

-- -- Collidable

checkCollision :: (Point, Point) -> (Point, Point) -> Bool
checkCollision (r1p1, r1p2) (r2p1, r2p2) = fst (r1p1) < fst (r2p2) && fst (r1p2) > fst (r2p1) && snd (r1p1) > snd (r2p2) && snd (r1p2) < snd (r2p1)

toHitBox :: Position -> Size -> (Point, Point)
toHitBox p@(pX, pY) (Size (sX, sY)) = (p, (pX + sX, pY + sY))

instance Collidable Airplane Airplane where
  collides
    Airplane {airplaneType = type1, airplanePos = pos1, airplaneSize = size1}
    Airplane {airplaneType = type2, airplanePos = pos2, airplaneSize = size2}
      | type1 == Player && type2 == Player = False
      | type1 == Player = checkCollision (toHitBox pos1 size1) (toHitBox pos2 size2)
      | otherwise = False

instance Collidable Projectile Airplane where
  collides
    Projectile {projectileOrigin = o, projectilePos = pPos, projectileSize = pSize}
    Airplane {airplaneType = t, airplanePos = aPos, airplaneSize = aSize}
      | o == Players && t == Player = False
      | o == Enemies && t /= Player = False
      | otherwise = checkCollision (toHitBox pPos pSize) (toHitBox aPos aSize)

instance Collidable Projectile Projectile where
  collides
    Projectile {projectileOrigin = o1, projectilePos = pos1, projectileSize = size1}
    Projectile {projectileOrigin = o2, projectilePos = pos2, projectileSize = size2}
      | o1 /= o2 = checkCollision (toHitBox pos1 size1) (toHitBox pos2 size2)
      | otherwise = False

-- Updateable

updatePosition :: Position -> Velocity -> Position
updatePosition (pX, pY) (Velocity (vX, vY)) = (pX + vX, pY + vY)

updateVelocity :: Velocity -> Velocity
updateVelocity (Velocity (x, y)) = Velocity (update x, update y)
  where
    update z
      | signum z == 1 = if z > 0.2 then z - 0.2 else 0.0
      | otherwise = if z < -0.2 then z + 0.2 else 0.0

instance Updateable Airplane where
  move airplane@Airplane {airplanePos = p, airplaneVelocity = v} = airplane {airplanePos = updatePosition p v, airplaneVelocity = updateVelocity v}

instance Updateable Projectile where
  move projectile@Projectile {projectilePos = p, projectileVelocity = v} = projectile {projectilePos = updatePosition p v}

-- Drawable

instance Drawable Airplane where
  draw Airplane {airplanePos = p, airplaneSprite = s} = uncurry translate p s

instance Drawable Projectile where
  draw Projectile {projectilePos = p, projectileSprite = s} = uncurry translate p s
