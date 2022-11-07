module Assets where

import Control.Monad
import Data.Char (toLower)
import Data.Map as Dict
import Data.Maybe
import Graphics.Gloss
import Model
import System.Directory
import System.FilePath

-- TODO Naming refactor
-- TODO values in Config.hs
-- TODO REORDER

assetsPath :: FilePath
assetsPath = "assets/"

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

loadAssets :: FilePath -> IO (String, Picture)
loadAssets filePath = do
  assets <- loadBMP filePath
  return (getFileName, assets)
  where
    -- only get filename from filePath -> assets/projectiles/bullet.bmp = bullet
    getFileName = takeWhile ('.' /=) $ takeFileName filePath

combinePath :: [FilePath] -> [[FilePath]] -> [[FilePath]]
combinePath [] _ = []
combinePath _ [] = []
combinePath (x : xs) (y : ys) = Prelude.map (x ++) y : combinePath xs ys -- TODO use higher order functions

getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = do
  allFiles <- listDirectory filePath
  filterM (doesDirectoryExist . (filePath </>)) allFiles

-- TODO enemySpriteRotation playerSpriteRotation
getTexture :: String -> Assets -> Picture
getTexture spriteName assetList = case Dict.lookup (Prelude.map toLower spriteName) assetList of
  Nothing -> rotate (-90) $ Scale 0.25 0.25 (color red $ Text spriteName) -- TODO player and enemy sprite rotation in config and scale
  Just x -> x

-- TODO MARK FIX THIS
getParticle :: String -> Particles -> Particle
getParticle key _map = fromMaybe Particle {particlePos = (0, 0), particleSize = (10, 10), particleInterval = 60, particleTimer = 60, particleSprites = [errorSprite key]} (Dict.lookup key _map)

fixImageOrigin :: Picture -> Size -> Picture
fixImageOrigin pic (width, height) = translate (width * 0.5) (height * (-0.5)) pic

errorSprite :: String -> Picture
errorSprite name = Scale 0.25 0.25 (color red $ Text name)