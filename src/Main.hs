module Main where

import Assets (getAssets)
import Controller
import Data.Map as Map
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Input
import Menu
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
  -- player1BMP <- loadBMP "assets/ships/player_01.bmp"
  -- player2BMP <- loadBMP "assets/ships/player_02.bmp"
  assets <- getAssets
  -- tile1BMP <- loadBMP "assets/tiles/tile_01.bmp"
  -- powerPackSprite1BMP <- loadBMP "assets/tiles/tile_0025.bmp"
  -- powerPackSprite2BMP <- loadBMP "assets/tiles/tile_0025x.bmp"
  -- projectile1BMP <- loadBMP "assets/projectiles/projectile_01.bmp"
  -- explosionPart1BMP <- loadBMP "assets/tiles/tile_0004.bmp"
  -- explosionPart2BMP <- loadBMP "assets/tiles/tile_0005.bmp"
  -- explosionPart3BMP <- loadBMP "assets/tiles/tile_0006.bmp"
  -- explosionPart4BMP <- loadBMP "assets/tiles/tile_0007.bmp"
  -- explosionPart5BMP <- loadBMP "assets/tiles/tile_0008.bmp"
  -- explosion2Part1BMP <- loadBMP "assets/tiles/tile_0004x.bmp"
  -- explosion2Part2BMP <- loadBMP "assets/tiles/tile_0005x.bmp"
  -- explosion2Part3BMP <- loadBMP "assets/tiles/tile_0006x.bmp"
  -- explosion2Part4BMP <- loadBMP "assets/tiles/tile_0007x.bmp"
  -- explosion2Part5BMP <- loadBMP "assets/tiles/tile_0008x.bmp"
  -- number1BMP <- loadBMP "assets/tiles/tile_0020.bmp"
  -- number2BMP <- loadBMP "assets/tiles/tile_0021.bmp"
  -- number3BMP <- loadBMP "assets/tiles/tile_0022.bmp"
  -- number4BMP <- loadBMP "assets/tiles/tile_0023.bmp"
  -- number5BMP <- loadBMP "assets/tiles/tile_0031.bmp"

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
    --(initialState (Map.fromList [("player1", player1BMP), ("player2", player2BMP), ("bullet", projectile1BMP), ("powerPackSprite1", powerPackSprite1BMP), ("powerPackSprite2", powerPackSprite2BMP), ("5", number5BMP), ("4", number4BMP), ("3", number3BMP), ("2", number2BMP), ("1", number1BMP), ("explosionPart1", explosionPart1BMP), ("explosionPart2", explosionPart2BMP), ("explosionPart3", explosionPart3BMP), ("explosionPart4", explosionPart4BMP), ("explosionPart5", explosionPart5BMP), ("explosion2Part1", explosion2Part1BMP), ("explosion2Part2", explosion2Part2BMP), ("explosion2Part3", explosion2Part3BMP), ("explosion2Part4", explosion2Part4BMP), ("explosion2Part5", explosion2Part5BMP)]))
    (initialState assets)
    view
    input
    step
