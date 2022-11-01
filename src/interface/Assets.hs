module Assets where

import Control.Monad
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

combinePath :: [FilePath] -> [[FilePath]] -> [[FilePath]]
combinePath [] _ = []
combinePath _ [] = []
combinePath (x : xs) (y : ys) = Prelude.map (x ++) y : combinePath xs ys

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

-- TODO enemySpriteRotation playerSpriteRotation
getTexture :: String -> Assets -> Picture
getTexture s m = case Dict.lookup s m of
  Nothing -> rotate (-90) $ Scale 0.25 0.25 (color red $ Text s)
  Just x -> x

-- TODO MARK FIX THIS
getParticle :: String -> Map String Particle -> Particle
getParticle key _map = fromMaybe Particle {particlePosition = (0, 0), particleSize = (10, 10), particleInterval = 60, particleTimer = 60, particleSprites = [Scale 0.25 0.25 (color red $ Text "error")]} (Dict.lookup key _map)

fixImageOrigin :: Picture -> Size -> Picture
fixImageOrigin pic (x, y) = translate (x * 0.5) (y * (-0.5)) pic