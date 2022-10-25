module Main where

import Controller
import Data.Map as Map
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

offset :: Int
offset = 100

main :: IO ()
main = do
  player1BMP <- loadBMP "assets/ships/player_01.bmp"
  player2BMP <- loadBMP "assets/ships/player_02.bmp"
  tile1BMP <- loadBMP "assets/tiles/tile_01.bmp"
  projectile1BMP <- loadBMP "assets/projectiles/projectile_01.bmp"
  let -- Or FullScreen
      -- Background color
      -- Frames per second
      -- Initial state
      -- View function
      -- Event function
      -- Step function
      -- background :: Color
      -- background = black

  playIO
    (InWindow "Nice Window" (1124, 868) (offset, offset))
    black
    fps
    (initialState (Map.fromList [("player1", player1BMP), ("player2", player2BMP), ("bullet", projectile1BMP), ("healthPack", tile1BMP)]))
    view
    input
    step
