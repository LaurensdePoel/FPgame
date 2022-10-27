-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Updates

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step seconds gs@Game {status = status', elapsedTime = elapsedTime'}
  | status' == InMenu = return $ ckeckInput gs
  | status' == InGame = return $ updateGameState $ gs {elapsedTime = updateTime}
  | otherwise = return gs
  where
    updateTime :: Time
    updateTime = elapsedTime' + seconds
