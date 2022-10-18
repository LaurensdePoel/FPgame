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

width, height, offset, fps :: Int
width = 1024
height = 768
offset = 100
fps = 60

main :: IO ()
main = do
  plane1 <- loadBMP "assets/ship_0004.bmp"
  let
  playIO
    (InWindow "Nice Window" (width, height) (offset, offset)) -- Or FullScreen
    black -- Background color
    fps -- Frames per second
    (initialState plane1) -- Initial state
    view -- View function
    input -- Event function
    step -- Step function

background :: Color
background = black
