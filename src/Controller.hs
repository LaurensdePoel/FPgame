-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (Char),
  )
import Model

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step seconds gstate
  | elapsedTime gstate + seconds > nO_SECS_BETWEEN_CYCLES =
    -- enough time has passed call new update
    return $ gstate {elapsedTime = elapsedTime gstate + 1}
  | otherwise =
    -- Just update the elapsed time
    return $ gstate {elapsedTime = elapsedTime gstate + seconds}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- (GameState _ _ _ _ (Player ((Airplane{airplanePos })))
-- {elapsedTime = elapsedTime gstate + 1}
changePositionFromPlane :: Airplane -> Airplane
changePositionFromPlane airplane = airplane {airplanePos = Position (1, 5)}

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'w') _ _ _) gs =
  gs
    { players =
        let pstate = players gs
         in pstate
              { airplanePos = moveUp (airplanePos pstate)
              }
    }
inputKey (EventKey (Char 's') _ _ _) gs =
  gs
    { players =
        let pstate = players gs
         in pstate
              { airplanePos = moveDown (airplanePos pstate)
              }
    }
inputKey (EventKey (Char 'a') _ _ _) gs =
  gs
    { players =
        let pstate = players gs
         in pstate
              { airplanePos = moveLeft (airplanePos pstate)
              }
    }
inputKey (EventKey (Char 'd') _ _ _) gs =
  gs
    { players =
        let pstate = players gs
         in pstate
              { airplanePos = moveRight (airplanePos pstate)
              }
    }
inputKey _ gs = gs -- Otherwise keep the same

moveUp :: Position -> Position
moveUp (Position (x, y)) = Position (x, y + 5)

moveDown :: Position -> Position
moveDown (Position (x, y)) = Position (x, y - 5)

moveLeft :: Position -> Position
moveLeft (Position (x, y)) = Position (x -5, y)

moveRight :: Position -> Position
moveRight (Position (x, y)) = Position (x + 5, y - 5)