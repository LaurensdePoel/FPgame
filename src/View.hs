-- | This module defines how to turn
--   the game state into a picture
module View where

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
  pictures
    [ walls,
      tmpPic gState
    ]
  where
    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 300 10

    wallColor = greyN 0.5
    walls = pictures [wall 200, wall (-200)]