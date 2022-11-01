module Main where

import Assets (getAssets)
import Config
import Controller
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Init
import Input
import View

main :: IO ()
main = do
  assets <- getAssets
  let
  playIO
    (InWindow "Nice Window" (screenWidth, screenHeight) (offset, offset))
    black
    fps
    (initialState assets)
    view
    input
    step
