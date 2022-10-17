-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (Char),
  )
import Model
import System.Random

type Radius = Float

type Position = (Float, Float)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step seconds gstate
  | elapsedTime gstate + seconds > nO_SECS_BETWEEN_CYCLES =
    -- We show a new random number
    return $ moveBall seconds gstate
  | otherwise =
    -- Just update the elapsed time
    return $ gstate {elapsedTime = elapsedTime gstate + seconds}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate =
  -- If the user presses a character key, reset the ball to the center
  gstate {ballLoc = (0, 0)}
inputKey _ gstate = gstate -- Otherwise keep the same

-- | Update the ball position using its current velocity.
moveBall ::
  Float -> -- The number of seconds since last update
  GameState -> -- The initial game state
  GameState -- A new game state with an updated ball position
moveBall seconds game = game {ballLoc = (x', y')}
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds
