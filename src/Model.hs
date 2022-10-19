{-# LANGUAGE DeriveAnyClass #-}

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

data Origin = Players | Enemies deriving(Eq)
class Collidable a where
  -- collides :: Collidable b => a -> b -> Bool
  collides ::  a -> a  -> Bool
data Airplane = Airplane
  { airplanePos :: Position,
    size :: Size,
    airplaneVelocity :: Velocity
  }

data Player = Player
  { playerAirplane :: Airplane --,
  -- powerUps :: [PowerUp]
  }
  -- deriving (Collidable)

data Enemy
  = Fighter
      { fighterPlane :: Airplane
      }
  | Kamikaze
      { kamikazePlane :: Airplane
      }
      deriving (Collidable)

data Projectile = Projectile
  { projectileType :: ProjectileType,
    projectilePos :: Position,
    projectileSize :: Size,
    projectileVelocity :: Velocity,
    damage :: Damage,
    origin :: Origin,
    sprite :: Picture
  }
  -- deriving (Collidable)

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
      plane = Airplane {airplanePos = Position (-10,30), size = Size (50,50), airplaneVelocity = Velocity (5,5)},
      tmpInt = 0
    }

checkCollision :: (Point, Point) -> (Point,Point) -> Bool
checkCollision (r1p1,r1p2) (r2p1,r2p2) = fst(r1p1) < fst(r2p2) && fst(r1p2) > fst(r2p1) && snd(r1p1) > snd(r2p2) && snd(r1p2) < snd(r2p1)

toBoundingBox :: Position -> Size -> (Point, Point)
toBoundingBox (Position p@(pX, pY)) (Size (sX, sY)) = (p, (pX + sX, pY + sY))

airplaneToBoundingBox :: Airplane -> (Point, Point)
airplaneToBoundingBox Airplane {airplanePos = p, size = s} = toBoundingBox p s

instance Collidable Player where
  collides Player { playerAirplane = pPlane } Fighter { fighterPlane = ePlane } = checkCollision (airplaneToBoundingBox pPlane) (airplaneToBoundingBox ePlane)
  collides Player { playerAirplane = pPlane } Kamikaze { kamikazePlane = ePlane } = checkCollision (airplaneToBoundingBox pPlane) (airplaneToBoundingBox ePlane)
  collides Player { playerAirplane = pPlane } Projectile { origin = o, projectilePos = projectilePos, projectileSize = projectileSize} 
        | o == Enemies = checkCollision (airplaneToBoundingBox pPlane) $ toBoundingBox projectilePos projectileSize
        | otherwise = False
  collides Player {} _ = False
-- instance Collidable Projectile where
--   collides Projectile {origin = o1, projectilePos = pPos, projectileSize = pSize} Projectile {origin = o2, projectilePos = p2Pos, projectileSize = p2Size} 
--                 | o1 /= o2 = checkCollision projectileBox $ toBoundingBox p2Pos p2Size
--                 | otherwise = False
--   collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Player { playerAirplane = plane } = collides' Players          
--   collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Fighter { fighterPlane = plane } = collides' Enemies  
--   collides Projectile {origin = o, projectilePos = pPos, projectileSize = pSize} Kamikaze { kamikazePlane = plane } = collides' Enemies  
--   collides Projectile {} _ = False
--     where
--       projectileBox = toBoundingBox pPos pSize
--       airplaneBox = airplaneToBoundingBox plane
--       collides' otherOrigin | o != otherOrigin = checkCollision projectileBox airplaneBox
--                     | otherwise = False



-- instance Collidable Enemy where
--   collides Fighter Projectile = undefined
--   collides Fighter Player = undefined
--   collides Fighter _ = False
--   collides Kamikaze Projectile = undefined
--   collides Kamikaze Player = undefined
--   collides Kamikaze _ = False

