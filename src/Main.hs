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
width = 1920
height = 1080
offset = 100
fps = 60

main :: IO ()
main =
  playIO
    (InWindow "Nice Window" (width, height) (offset, offset)) -- Or FullScreen
    black -- Background color
    fps -- Frames per second
    initialState -- Initial state
    view -- View function
    input -- Event function
    step -- Step function

background :: Color
background = black
