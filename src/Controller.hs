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

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate = gstate {tmpInt = 1} -- If the user presses c, do something
inputKey _ gstate = gstate -- Otherwise keep the same