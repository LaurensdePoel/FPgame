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
import LoadLevels (getLevelsInJSON)
import View

-- | The entery of Haskell game
main :: IO ()
main = do
  jsonLevel <- getLevelsInJSON
  assets <- getAssets
  let levels = Prelude.map (`levelConverter` assets) jsonLevel
  let levelSelectMenu = createLevelSelectmenu levels
  let
  playIO
    (InWindow "Nice Window" (screenWidth, screenHeight) (offset, offset))
    black
    fps
    (initialState assets levels levelSelectMenu)
    view
    input
    step
