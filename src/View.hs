-- | This module defines how to turn
--   the game state into a picture
module View where

import Config as C
import Drawable
import Graphics.Gloss
import Model

-- | Function 'view' creates a IO picture from the GameState
view :: GameState -> IO Picture
view = return . render

-- | Converts the GameState to a Picture
render ::
  -- | The game state to render.
  GameState ->
  -- | A picture of this game state.
  Picture
render gs@GameState {status = _status} = case _status of
  InMenu -> drawMenu
  InGame -> drawGame
  where
    drawMenu :: Picture
    drawMenu = draw (menu gs)

    drawGame :: Picture
    drawGame = pictures $ mapDraw (projectiles gs) ++ mapDraw (players gs) ++ mapDraw (enemies gs) ++ mapDraw (powerUps gs) ++ mapDraw (particles gs) ++ [color white $ rectangleWire (fromIntegral C.screenWidth) (fromIntegral C.screenHeight)]
