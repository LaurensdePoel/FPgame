module Assets where

import Data.Map (Map, fromList)
import Graphics.Gloss (Picture, loadBMP)
import System.Directory

baseAssets :: String
baseAssets = "assets/"

getAssets :: IO (Map String Picture)
getAssets = do
  assets <- listDirectory baseAssets
  sprites <- mapM loadAssets assets
  return $ fromList sprites

loadAssets :: FilePath -> IO (String, Picture)
loadAssets fileName = do
  assets <- loadBMP (baseAssets ++ fileName)
  return (takeWhile ('.' /=) fileName, assets)