-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . render

viewPure :: GameState -> Picture
viewPure = undefined

drawAirplane :: Airplane -> Picture
drawAirplane Airplane {airplanePos = Position pos, airplaneSprite = sprite} = uncurry translate pos sprite

render ::
  -- | The game state to render.
  GameState ->
  -- | A picture of this game state.
  Picture
render gState =
  pictures
    [ walls,
      -- {airplane{playerAirplane {player gState}}
      drawAirplane (playerAirplane (player gState))
      --airplaneSprite (playerAirplane (player gState))
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

    p1Airplane :: Airplane -> Picture
    p1Airplane (Airplane (Position pos) _ _ _ pic) = uncurry translate pos pic