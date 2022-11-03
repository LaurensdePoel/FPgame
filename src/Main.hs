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
import System.Random
import View

-- | The entery of Haskell game
main :: IO ()
main = do
  -- B.writeFile "level.json" (encode myFoo)
  -- raw <- B.readFile "level.json"
  -- print $ eitherDecode raw :: IO (Either String [ResLevel])

  -- BS.writeFile "level.json" (encode myLevel)
  -- Get JSON data and decode it
  d <- (eitherDecode <$> getJSON) :: IO (Either String ResLevel)
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice. In this case, just print it.
  case d of
    Left err -> Prelude.putStrLn err
    Right ps -> print ps
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
