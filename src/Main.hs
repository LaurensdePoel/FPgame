module Main where

import Controller
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import View

-- import Temp

-- -- in gloss main function will always be one line
-- main :: IO ()
-- main = animate window background frame
--     where
--         frame :: Flaot -> Picture
--         frame seconds = render(render initialState)

width, height, offset :: Int
width = 1024
height = 768
offset = 100

main :: IO ()
main = do
  player1BMP <- loadBMP "assets/ships/player_01.bmp"
  tile1BMP <- loadBMP "assets/tiles/tile_01.bmp"
  projectile1BMP <- loadBMP "assets/projectiles/projectile_01.bmp"
  let
  playIO
    (InWindow "Nice Window" (width, height) (offset, offset)) -- Or FullScreen
    black -- Background color
    fps -- Frames per second
    (initialState [player1BMP, tile1BMP, projectile1BMP]) -- Initial state
    view -- View function
    input -- Event function
    step -- Step function

-- background :: Color
-- background = black
