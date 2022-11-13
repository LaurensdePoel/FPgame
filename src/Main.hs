-- | This module defines the main
module Main where

import Assets (getAssets)
import Config
import Controller
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
    (InWindow "FP - Haskell - Game project" (screenWidth, screenHeight) (offset, offset))
    black
    fps
    (initialState assets levels (levelSelectMenu assets))
    view
    input
    step
