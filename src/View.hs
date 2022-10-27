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
render gs =
  pictures xs
  where
    xs = draw (menu gs) : mapDraw (projectiles gs) ++ mapDraw (players gs) ++ mapDraw (enemies gs)

--fieldsPic = fields (menu gs)