-- | This module defines the main
module Main where

import Assets (getAssets)
import Config
import Controller
import Data.Aeson
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as B
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Init
import Input
import Level
import View

-- | The entery of Haskell game
main :: IO ()
main = do
  -- loadLevel "level.json"
  -- writeJSONLevelToJson "level.json"
  -- print plane
  jsonLevel <- loadLevel "level.json"
  assets <- getAssets
  let level = levelConverter jsonLevel assets
  let
  playIO
    (InWindow "Nice Window" (screenWidth, screenHeight) (offset, offset))
    black
    fps
    (initialState assets [level])
    view
    input
    step
