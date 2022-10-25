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
import Updates

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step seconds gstate
  | elapsedTime gstate + seconds > 0.01666667 -- (fromIntegral fps :: Float)
  -- enough time has passed call new update
    =
    return $ updateGameState $ gstate {elapsedTime = 0}
  --return $ gstate {elapsedTime = 0, players = move $ updatePlayerVelocity (keys gstate) (players gstate), projectiles = destroy (map move . projectiles $ snd updatedPlayer)}
  | otherwise =
    -- Just update the elapsed time
    return $ gstate {elapsedTime = elapsedTime gstate + seconds}

-- update :: Float -> World -> World
-- update _ world
--     | S.member (SpecialKey KeySpace) (keys world) = world { counter = 1 + counter world }
--     | otherwise = world { counter = 0 }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey k Down _ _) gs = gs {pressedKeys = S.insert k (pressedKeys gs)}
inputKey (EventKey k Up _ _) gs = gs {pressedKeys = S.delete k (pressedKeys gs)}
inputKey _ gs = gs -- Otherwise keep the same
