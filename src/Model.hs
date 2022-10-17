-- | This module contains the data types
--   which represent the state of the game
module Model where

data GameState = Game
  { elapsedTime :: Float,
    -- | Pong ball (x, y) location.
    ballLoc :: (Float, Float),
    ballVel :: (Float, Float),
    -- | Left player paddle height.
    -- Zero is the middle of the screen.
    player1 :: Float,
    -- | Right player paddle height.
    player2 :: Float
  }
  deriving (Show)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

initialState :: GameState
initialState =
  Game
    { elapsedTime = 0,
      ballLoc = (-10, 30),
      ballVel = (1, -3),
      player1 = 40,
      player2 = -80
    }