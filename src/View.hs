-- | This module defines how to turn
--   the game state into a picture
module View where

import Drawable
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . render

viewPure :: GameState -> Picture
viewPure = undefined

render ::
  -- | The game state to render.
  GameState ->
  -- | A picture of this game state.
  Picture
render gState =
  pictures xs
  where
    xs = mapDraw (projectiles gState) ++ mapDraw (players gState) ++ mapDraw (enemies gState) ++ mapDraw (powerUps gState) ++ mapDraw (particles gState)
