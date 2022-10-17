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
render game =
  pictures
    [ ball,
      walls,
      mkPaddle rose 120 $ player1 game,
      mkPaddle orange (-120) $ player2 game
    ]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y =
      pictures
        [ translate x y $ color col $ rectangleSolid 26 86,
          translate x y $ color paddleColor $ rectangleSolid 20 80
        ]

    paddleColor = light (light blue)
