module Assets where

import Control.Monad (filterM)
import Data.Char (toLower)
import Data.Map as Dict (fromList, lookup)
import Data.Maybe (fromMaybe)
import Graphics.Gloss
  ( Picture (Scale, Text),
    color,
    loadBMP,
    red,
    translate,
  )
import Model
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))

-- | Assets folder location
assetsPath :: FilePath
assetsPath = "assets/"

-- | Get the asset map
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

-- | Load asset (asset name and create picture)
loadAssets :: FilePath -> IO (String, Picture)
loadAssets filePath = do
  assets <- loadBMP filePath
  return (getFileName, assets)
  where
    -- only get filename from filePath -> assets/projectiles/bullet.bmp = bullet
    getFileName = takeWhile ('.' /=) $ takeFileName filePath

-- | Combine the path of x in xs with all paths in ys of yss
combinePath :: [FilePath] -> [[FilePath]] -> [[FilePath]]
combinePath [] _ = []
combinePath _ [] = []
combinePath (x : xs) (ys : yss) = Prelude.map (x ++) ys : combinePath xs yss

-- | Get all directories of the given file path
getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = do
  allFiles <- listDirectory filePath
  filterM (doesDirectoryExist . (filePath </>)) allFiles

-- | Get the texture from the asset map (if not exists: it creates an error texture)
getTexture :: String -> Assets -> Picture
getTexture spriteName assets =
  case Dict.lookup (Prelude.map toLower spriteName) assets of
    Nothing -> errorSprite spriteName
    Just x -> x

-- | Get the particle from the particle Map (if not exists: it creates an error particle)
getParticle :: String -> Particles -> Particle
getParticle key particleMap' =
  fromMaybe
    Particle {particlePos = (0, 0), particleSize = (10, 10), particleInterval = 60, particleTimer = 60, particleSprites = [errorSprite key]}
    (Dict.lookup key particleMap')

-- | Corrects the image origin based on the given Size
fixImageOrigin :: Picture -> Size -> Picture
fixImageOrigin pic (width, height) = translate (width * 0.5) (height * (-0.5)) pic

-- | Create a error sprite, where the text is the given string
errorSprite :: String -> Picture
errorSprite name = Scale 0.25 0.25 (color red $ Text name)