{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module LoadLevels where

import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Data.List (sort)
import GHC.Generics (Generic)
import Model
import System.Directory

-- * Models

data LevelJSON = LevelJSON
  { resLevelNr :: Int,
    resLevelBackgroundName :: String,
    resWaves :: [WaveJSON]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data WaveJSON = WaveJSON
  { resEnemiesInWave :: [AirplaneJSON],
    resWaveTimer :: Time
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data AirplaneJSON = AirplaneJSON
  { resAirplaneType :: AirPlaneType,
    resAirplanePos :: Position
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- * Code

levelsPath :: FilePath
levelsPath = "levels/"

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

-- | Read a single Level.JSON and converts it the record 'LevelJson'.
-- If there is an error with the format of the JSON file it wil produce a level with the number (-1) wich indicates a faulty level.
loadLevel :: FilePath -> IO LevelJSON
loadLevel filePath = do
  d <- (eitherDecode <$> getJSON (levelsPath ++ filePath)) :: IO (Either String LevelJSON)

  case d of
    Left err -> do
      print err
      return $ LevelJSON (-1) "" []
    Right levelJSON ->
      do
        return $ checkLevelJson levelJSON
  where
    checkLevelJson :: LevelJSON -> LevelJSON
    checkLevelJson level@LevelJSON {resLevelNr = _resLevelNr, resWaves = _resWaves}
      | _resLevelNr < 0 = LevelJSON (-1) "" [] -- level number below 0
      | otherwise = level

-- | loops trough all the Level.JSON files an convert them to a 'LevelJSON' record.
getLevelsInJSON :: IO [LevelJSON]
getLevelsInJSON = do
  levelFileNames <- listDirectory levelsPath
  mapM loadLevel $ sort levelFileNames
