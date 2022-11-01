-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Updates

-- TODO Naming refactor
-- TODO values in Config.hs

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step seconds gs@GameState {status = status', elapsedTime = elapsedTime'}
  | status' == InMenu = return $ updateMenu gs
  | status' == InGame = return $ updateGameState $ gs {elapsedTime = updateTime}
  | otherwise = return gs
  where
    updateTime :: Time
    updateTime = elapsedTime' + seconds
