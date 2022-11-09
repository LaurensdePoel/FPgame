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

levelsPath :: FilePath
levelsPath = "levels/"

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

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

getLevelsInJSON :: IO [LevelJSON]
getLevelsInJSON = do
  levelFileNames <- listDirectory levelsPath
  mapM loadLevel $ sort levelFileNames

-- myLevel :: LevelJSON
-- myLevel = LevelJSON 1 [WaveJSON [AirplaneJSON Fighter (300, -300), AirplaneJSON Fighter (300, 300)] 50, WaveJSON [AirplaneJSON Kamikaze (300, -300), AirplaneJSON Kamikaze (300, 300)] 50]

-- writeJSONLevelToJson :: FilePath -> IO ()
-- writeJSONLevelToJson filePath = do
--   B.writeFile filePath (encode myLevel