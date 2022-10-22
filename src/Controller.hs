-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (Char),
  )
import Graphics.Gloss.Interface.IO.Interact (KeyState (..))
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
changePositionFromPlane airplane = airplane {airplanePos = (1, 5)}

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char char) _ _ _) gs
  | char == 'w' = move (0, 5)
  | char == 's' = move (0, -5)
  | char == 'a' = move (-5, 0)
  | char == 'd' = move (5, 0)
  where
    move addToPosition =
      gs
        { players =
            let playerState = players gs
             in playerState
                  { airplanePos = airplanePos playerState + addToPosition
                  }
        }
inputKey _ gs = gs -- Otherwise keep the same

inputKeyV2 :: Event -> GameState -> GameState
inputKeyV2 (EventKey k Down _ _) gs = gs {keys = S.insert k (keys gs)}
inputKeyV2 (EventKey k Up _ _) gs = gs {keys = S.delete k (keys gs)}
inputKeyV2 _ gs = gs -- Otherwise keep the same