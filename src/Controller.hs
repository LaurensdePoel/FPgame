-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (Char),
  )
import Model
import Updates

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step seconds gstate
  | elapsedTime gstate + seconds > 0.01666667 -- (fromIntegral fps :: Float)
  -- enough time has passed call new update
    =
      return $ updateGameState $ gstate {elapsedTime = 0}
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
            let playerState = head $ players gs
             in [ playerState
                    { airplanePos = addValues addToPosition (airplanePos playerState)
                    }
                ]
        }
    addValues addPos oldPos = addPos + oldPos
inputKey _ gs = gs -- Otherwise keep the same
