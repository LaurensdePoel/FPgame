-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (Char),
  )
import Graphics.Gloss.Interface.IO.Interact (KeyState (..))
import Model

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step seconds gstate
  | elapsedTime gstate + seconds > 0.01666667 -- (fromIntegral fps :: Float)
  -- enough time has passed call new update
    =
    return $ gstate {elapsedTime = 0, players = move $ updatePlayerVelocity (keys gstate) (players gstate), projectiles = destroy (map move . projectiles $ snd updatedPlayer)}
  --return $ gstate {elapsedTime = 0, players =  (fst updatedPlayer), projectiles = destroy (map move . projectiles $ snd updatedPlayer)}
  | otherwise =
    -- Just update the elapsed time
    return $ gstate {elapsedTime = elapsedTime gstate + seconds}
  where
    updatedPlayer = shoot (move $ players gstate) gstate

-- update :: Float -> World -> World
-- update _ world
--     | S.member (SpecialKey KeySpace) (keys world) = world { counter = 1 + counter world }
--     | otherwise = world { counter = 0 }

--updatePlayerVelocity :: Key -> Airplane -> Airplane
updatePlayerVelocity :: S.Set Key -> Airplane -> Airplane
updatePlayerVelocity activeKeys airplane
  -- | S.member (SpecialKey KeyUp) activeKeys =
  --   airplane {airplanePos = (0, 0)}
  | S.member (Char 'w') activeKeys =
    airplane {airplaneVelocity = airplaneVelocity airplane + (0, 5)}
  | S.member (Char 'a') activeKeys =
    airplane {airplaneVelocity = airplaneVelocity airplane + (-5, 0)}
  | S.member (Char 's') activeKeys =
    airplane {airplaneVelocity = airplaneVelocity airplane + (0, -5)}
  | S.member (Char 'd') activeKeys =
    airplane {airplaneVelocity = airplaneVelocity airplane + (5, 0)}
  | otherwise = airplane

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey k Down _ _) gs = gs {keys = S.insert k (keys gs)}
inputKey (EventKey k Up _ _) gs = gs {keys = S.delete k (keys gs)}
inputKey _ gs = gs -- Otherwise keep the same
