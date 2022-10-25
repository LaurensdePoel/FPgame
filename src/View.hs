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

-- drawAirplane :: Airplane -> Picture
-- drawAirplane Airplane {airplanePos = pos, airplaneSprite = sprite} = uncurry translate pos sprite

render ::
  -- | The game state to render.
  GameState ->
  -- | A picture of this game state.
  Picture
render gState =
  pictures xs
  where
    xs = draw (window gState) : mapDraw (projectiles gState) ++ mapDraw (players gState) ++ mapDraw (enemies gState) ++ points

    points = [point 0 0, point 200 200, point 200 (-200), point (-200) (-200), point (-200) 200]
    point x y = translate x y $ color red $ rectangleSolid 1 1
