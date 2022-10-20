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
inputKey (EventKey (Char w) _ _ _) gstate = gstate --{player GameState = a {playerAirplane a} = b} -- If the user presses c, do something
inputKey _ gstate = gstate -- Otherwise keep the same

-- handleKeys :: Event -> GameState -> GameState
-- handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
--   gs {direction = West, heading = FacingWest}
-- handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
--   gs {direction = East, heading = FacingEast}
-- handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gs =
--   gs
--     { speedY =
--         if isCollision gs (fst (position gs), snd (position gs) + speedY gs) '*'
--           then 6
--           else (-6)
--     }
-- handleKeys _ gs = gs {direction = None}