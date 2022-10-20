{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data Status = InMenu | InGame

newtype Position = Position Point

newtype Size = Size Point

data Velocity = Velocity Point

data ProjectileType = None | Gun | DoubleGun | Rocket

newtype Damage = Damage Int

data Origin = Players | Enemies deriving (Eq)

class Collidable a b where
  collides :: a -> b -> Bool

class Updateable a where
  move :: a -> a

data Airplane = Airplane
  { airplanePos :: Position,
    size :: Size,
    airplaneVelocity :: Velocity
  }

data Player = Player
  { playerAirplane :: Airplane --,
  -- powerUps :: [PowerUp]
  }

data Enemy
  = Fighter
      { fighterPlane :: Airplane
      }
  | Kamikaze
      { kamikazePlane :: Airplane
      }

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
    plane :: Airplane, -- tmp
    tmpInt :: Int
    --level :: Level,
    --projectiles :: [Projectile],
    --players :: [Player],
    --powerUP :: [PowerUp]
  }

-- deriving (Show)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

initialState :: GameState
initialState =
  Game
    { elapsedTime = 0,
      status = InGame,
      plane = Airplane {airplanePos = Position (-10, 30), size = Size (50, 50), airplaneVelocity = Velocity (5, 5)},
      tmpInt = 0
    }

-- Collidable

checkCollision :: (Point, Point) -> (Point, Point) -> Bool
checkCollision (r1p1, r1p2) (r2p1, r2p2) = fst (r1p1) < fst (r2p2) && fst (r1p2) > fst (r2p1) && snd (r1p1) > snd (r2p2) && snd (r1p2) < snd (r2p1)

toBoundingBox :: Position -> Size -> (Point, Point)
toBoundingBox (Position p@(pX, pY)) (Size (sX, sY)) = (p, (pX + sX, pY + sY))

airplaneToBoundingBox :: Airplane -> (Point, Point)
airplaneToBoundingBox Airplane {airplanePos = p, size = s} = toBoundingBox p s

instance Collidable Player Enemy where
  collides Player {playerAirplane = pPlane} Fighter {fighterPlane = ePlane} = checkCollision (airplaneToBoundingBox pPlane) (airplaneToBoundingBox ePlane)
  collides Player {playerAirplane = pPlane} Kamikaze {kamikazePlane = ePlane} = checkCollision (airplaneToBoundingBox pPlane) (airplaneToBoundingBox ePlane)

instance Collidable Projectile Enemy where
  collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Fighter {fighterPlane = fPlane}
    | o /= Enemies = checkCollision (toBoundingBox pPos pSize) (airplaneToBoundingBox fPlane)
    | otherwise = False
  collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Kamikaze {kamikazePlane = kPlane}
    | o /= Enemies = checkCollision (toBoundingBox pPos pSize) (airplaneToBoundingBox kPlane)
    | otherwise = False

instance Collidable Projectile Player where
  collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Player {playerAirplane = pPlane}
    | o /= Players = checkCollision (toBoundingBox pPos pSize) (airplaneToBoundingBox pPlane)
    | otherwise = False

instance Collidable Projectile Projectile where
  collides Projectile {origin = o1, projectilePos = p1Pos, projectileSize = p1Size} Projectile {origin = o2, projectilePos = p2Pos, projectileSize = p2Size}
    | o1 /= o2 = checkCollision (toBoundingBox p1Pos p1Size) (toBoundingBox p2Pos p2Size)
    | otherwise = False

-- Updateable

updatePosition :: Position -> Velocity -> Position
updatePosition (Position (pX, pY)) (Velocity (vX, vY)) = Position (pX * vX, pY * vY) -- TODO p * v

instance Updateable Player where
  move Player {} = undefined

instance Updateable Enemy where
  move Fighter {} = undefined
  move Kamikaze {} = undefined

instance Updateable Projectile where
  move object@Projectile {projectilePos = p, projectileVelocity = v} = object {projectilePos = updatePosition p v}
