-- | This module contains the data types
--   which represent the state of the game
module Model where

data Status = InMenu | InGame

data Position = Position Float Float

data PlaneSize = PlaneSize Float Float

data Velocity = Velocity Float Float

data Airplane = Plane
  { position :: Position,
    size :: PlaneSize,
    velocity :: Velocity
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
      plane = Plane {position = Position (-10) 30, size = PlaneSize 50 50, velocity = Velocity 5 5},
      tmpInt = 0
    }