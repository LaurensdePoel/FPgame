{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module LoadLevels where

import Assets
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Data.List (sort)
import Data.Map as Dict
import GHC.Generics (Generic)
import Graphics.Gloss
import Model
import System.Directory

data LevelJSON = LevelJSON
  { resLevelNr :: Int,
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
    Left err -> error err
    Right levelJSON -> do return levelJSON

getLevelsInJSON :: IO [LevelJSON]
getLevelsInJSON = do
  levelFileNames <- listDirectory levelsPath
  mapM loadLevel $ sort levelFileNames

-- ------------------------------------------------

backgroundPath :: FilePath
backgroundPath = "backgrounds/"

loadBackground :: FilePath -> IO BackgroundJSON
loadBackground filePath = do
  d <- (eitherDecode <$> getJSON (backgroundPath ++ filePath)) :: IO (Either String BackgroundJSON)
  case d of
    Left err -> error err
    Right backgroundJSON -> do return backgroundJSON

getBackgroundsInJSON :: IO [BackgroundJSON]
getBackgroundsInJSON = do
  backgroundFileNames <- listDirectory backgroundPath
  mapM loadBackground $ sort backgroundFileNames

createBackgroundMap :: [BackgroundJSON] -> Backgrounds
createBackgroundMap bgJSON = Prelude.foldr convertToMap (Dict.empty :: Backgrounds) bgJSON
  where
    convertToMap (BackgroundJSON key value) bgMap = Dict.insert key value bgMap

-- change m ns c = foldr (\k mp -> Dict.insert k c mp) m ns

-- getBackgroundsInBMP :: Assets -> [BackgroundJSON] -> Assets
-- getBackgroundsInBMP assets bgJSON = Prelude.foldr (\(k, v) mp -> Dict.insert k v mp) assets $ Prelude.map convert bgJSON
--   where
--     convert :: BackgroundJSON -> (String, Picture)
--     convert (BackgroundJSON name bgData) = (name, pictures $ Prelude.map (\number -> getTexture (show $ number - 1) assets) bgData)

data BackgroundJSON = BackgroundJSON
  { resBackgroundName :: String,
    resBackgroundData :: [Int]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- myLevel :: LevelJSON
-- myLevel = LevelJSON 1 [WaveJSON [AirplaneJSON Fighter (300, -300), AirplaneJSON Fighter (300, 300)] 50, WaveJSON [AirplaneJSON Kamikaze (300, -300), AirplaneJSON Kamikaze (300, 300)] 50]

-- writeJSONLevelToJson :: FilePath -> IO ()
-- writeJSONLevelToJson filePath = do
--   B.writeFile filePath (encode myLevel)