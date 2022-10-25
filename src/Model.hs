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

--This is unnecessary
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

--   (-) :: Position -> Position -> Position
--   (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
--   (*) :: Position -> Position -> Position
--   (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
--   signum :: Position -> Position
--   signum (x, y) = (signum x, signum y)
--   abs :: Position -> Position
--   abs (x, y) = (abs x, abs y)
--   negate :: Position -> Position
--   negate (x, y) = (negate x, negate y)
--   fromInteger :: Integer -> Position
--   fromInteger x = (fromInteger x, fromInteger x)

data ProjectileType = None | Gun | DoubleGun | Rocket

newtype Damage = Damage Int

data Origin = Players | Enemies deriving (Eq)

-- firerate timelastshot
data FireRate = Single Time | Burst Time

newtype ScreenBox = ScreenBox (Point, Point)

data AirPlaneType = Player1 | Player2 | Fighter | Kamikaze deriving (Eq)

-- class Collidable a b where
--   collides :: a -> b -> Bool

-- class Updateable a where
--   move :: a -> a
--   shoot :: a -> GameState -> (a, GameState)
--   destroy :: [a] -> [a]

--   updateAll :: a -> GameState -> a

-- updateAll = shoot . move

-- class Drawable a where
--   draw :: a -> Picture

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
                    projectileDamage = Damage 30,
                    projectileOrigin = Players,
                    projectileSprite = rotate 90 $ getTexture "bullet" assetlist
                  },
              airplaneSprite = rotate 90 $ getTexture "player1" assetlist
            },
          Airplane
            { airplaneType = Player2,
              airplanePos = (-200, 0),
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
                    projectileDamage = Damage 30,
                    projectileOrigin = Players,
                    projectileSprite = rotate 90 $ getTexture "bullet" assetlist
                  },
              airplaneSprite = rotate 90 $ getTexture "player2" assetlist
            }
        ],
      enemies = [],
      tmpInt = 0,
      pressedKeys = S.empty,
      projectiles = [],
      window = ScreenBox ((-300.0, 300.0), (300.0, -300.0))
    }

getTexture :: String -> Map String Picture -> Picture
getTexture s m = case Map.lookup s m of
  Nothing -> Text "error"
  Just x -> x

-- -- Collidable

-- checkCollision :: (Point, Point) -> (Point, Point) -> Bool
-- checkCollision (r1p1, r1p2) (r2p1, r2p2) = fst (r1p1) < fst (r2p1) + fst (r2p2) && fst (r1p1) + fst (r1p2) > fst (r2p1) && snd (r1p1) < snd (r2p1) + snd (r2p2) && snd (r1p2) + snd (r1p1) > snd (r2p1)

-- toHitBox :: Position -> Size -> (Point, Point)
-- toHitBox p@(pX, pY) (Size (sX, sY)) = (p, (pX + sX, pY + sY))

-- instance Collidable Airplane Airplane where
--   collides
--     Airplane {airplaneType = type1, airplanePos = pos1, airplaneSize = size1}
--     Airplane {airplaneType = type2, airplanePos = pos2, airplaneSize = size2}
--       | type1 == Player && type2 == Player = False
--       | type1 == Player = checkCollision (toHitBox pos1 size1) (toHitBox pos2 size2)
--       | otherwise = False

-- instance Collidable Projectile Airplane where
--   collides
--     Projectile {projectileOrigin = o, projectilePos = pPos, projectileSize = pSize}
--     Airplane {airplaneType = t, airplanePos = aPos, airplaneSize = aSize}
--       | o == Players && t == Player = False
--       | o == Enemies && t /= Player = False
--       | otherwise = checkCollision (toHitBox pPos pSize) (toHitBox aPos aSize)

-- instance Collidable Projectile Projectile where
--   collides
--     Projectile {projectileOrigin = o1, projectilePos = pos1, projectileSize = size1}
--     Projectile {projectileOrigin = o2, projectilePos = pos2, projectileSize = size2}
--       | o1 /= o2 = checkCollision (toHitBox pos1 size1) (toHitBox pos2 size2)
--       | otherwise = False

-- Updateable

-- updatePosition :: Position -> Velocity -> Position
-- updatePosition (pX, pY) (Velocity (vX, vY)) = (pX + vX, pY + vY)

-- updateVelocity :: Velocity -> Velocity
-- updateVelocity (Velocity (x, y)) = Velocity (update x, update y)
--   where
--     update z
--       | signum z == 1 = if z > 0.2 then z - 0.2 else 0.0
--       | otherwise = if z < -0.2 then z + 0.2 else 0.0

-- readyToShoot' :: Time -> Time -> Airplane -> (Bool, Airplane)
-- readyToShoot' x t a
--   | t > x = (True, a {timeLastShot = 0.0})
--   | otherwise = (False, a {timeLastShot = t + 1.0})

-- readyToShoot :: Airplane -> (Bool, Airplane)
-- readyToShoot a@Airplane {fireRate = r, timeLastShot = t} = case r of
--   Single x -> readyToShoot' x t a
--   Burst x -> readyToShoot' x t a

-- instance Updateable Airplane where
--   -- updateAll :: Airplane -> GameState -> Airplane
--   -- updateAll x g = shoot (move x) g

--   move :: Airplane -> Airplane
--   move airplane@Airplane {airplanePos = p, airplaneVelocity = v} = airplane {airplanePos = updatePosition p v, airplaneVelocity = updateVelocity v}

--   destroy :: [Airplane] -> [Airplane]
--   destroy airplanes = filter alive airplanes
--     where
--       alive Airplane {airplaneHealth = h} = h > 0

--   shoot :: Airplane -> GameState -> (Airplane, GameState)
--   shoot
--     airplane@Airplane {airplanePos = p, airplaneSize = s, airplaneProjectile = ap}
--     g@Game {projectiles = ps}
--       | ready = (a, g {projectiles = ap' : ps})
--       | otherwise = (a, g)
--       where
--         (ready, a) = readyToShoot airplane
--         ap' = ap {projectilePos = p}

-- instance Updateable Projectile where
--   -- updateAll :: Projectile -> Projectile
--   -- updateAll = move

--   move :: Projectile -> Projectile
--   move projectile@Projectile {projectilePos = p, projectileVelocity = v} = projectile {projectilePos = updatePosition p v}

--   destroy :: [Projectile] -> [Projectile]
--   destroy ps = filter inBounds ps
--     where
--       inBounds Projectile {projectilePos = p, projectileSize = s}
--         | checkCollision (toHitBox p s) ((-1000.0, -1000.0), (1000.0, 1000.0)) = True -- TODO: update to actual screen size + a little bit extra (And not hard coded)
--         | otherwise = False -- filter (\Projectile {projectilePos = p'} -> p' /= p) ps

-- -- Drawable

-- instance Drawable Airplane where
--   draw Airplane {airplanePos = p, airplaneSprite = s} = uncurry translate p s

-- instance Drawable Projectile where
--   draw Projectile {projectilePos = p, projectileSprite = s} = uncurry translate p s
