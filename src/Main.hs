-- | This module defines the main
module Main where

import Assets (getAssets)
import Config as C
import Controller (step)
import Graphics.Gloss (Display (InWindow), black)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Init (createLevelSelectmenu, initialState)
import Input (input)
import Level (levelConverter)
import LoadLevels (getLevelsInJSON)
import View (view)

-- | The entery of Haskell game
main :: IO ()
main = do
  jsonLevel <- getLevelsInJSON
  assets <- getAssets
  let levels = Prelude.map (`levelConverter` assets) jsonLevel
  let levelSelectMenu = createLevelSelectmenu levels
  let
  playIO
    (InWindow "FP - Haskell - Game project" (C.screenWidth, C.screenHeight) (C.offset, C.offset))
    black
    C.fps
    (initialState assets levels (levelSelectMenu assets))
    view
    input
    step
