module Assets where

import Control.Monad
import Data.Map (Map, fromList)
import Graphics.Gloss (Picture, loadBMP)
import System.Directory
import System.FilePath

assetsPath :: FilePath
assetsPath = "assets/"

type Assets = (Map String Picture)

getAssets :: IO Assets
getAssets = do
  -- get all subdirectories in current path
  pathDirs <- getDirectories assetsPath
  -- convert subdirectories in relative path
  completePathDirs <- mapM (\contents -> return $ assetsPath </> contents ++ "/") pathDirs
  -- subAssetsNames :: [[FilePath]] - get all filenames in directories
  subAssetsNames <- mapM listDirectory completePathDirs
  -- make 1 list of all relative filepaths
  let allAssets = concat $ combinePath completePathDirs subAssetsNames
  -- load all sprites
  sprites <- mapM loadAssets allAssets
  return $ fromList sprites

combinePath :: [FilePath] -> [[FilePath]] -> [[FilePath]]
combinePath [] _ = []
combinePath _ [] = []
combinePath (x : xs) (y : ys) = map (x ++) y : combinePath xs ys

getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = do
  allFiles <- listDirectory filePath
  filterM (doesDirectoryExist . (filePath </>)) allFiles

loadAssets :: FilePath -> IO (String, Picture)
loadAssets filePath = do
  assets <- loadBMP filePath
  return (getFileName, assets)
  where
    -- only leave filename -> assets/projectiles/bullet.bmp = bullet
    getFileName = takeWhile ('.' /=) $ takeFileName filePath
