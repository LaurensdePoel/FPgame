{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Level where

import Assets (combinePath, fixImageOrigin, getTexture)
import qualified Config as C
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Graphics.Gloss.Interface.IO.Animate (Picture (Pictures), rotate)
import Model
import System.Directory

-- | The function 'nextwave' sets the nextwave as currentwave. If there are no more waves this functions does nothing.
nextWave :: GameState -> GameState
nextWave gs@GameState {currentLevel = _currentLevel, enemies = _enemies}
  | ifAllWavesCleared = gs -- do nothing if all waves are cleared --TODO better if we can disable timer
  | otherwise = gs {enemies = _enemies ++ spawnNextWave, currentLevel = _currentLevel {waves = removeWaveAfterSpawn}}
  where
    ifAllWavesCleared :: Bool
    ifAllWavesCleared = null (waves _currentLevel)

    spawnNextWave :: [Enemy]
    spawnNextWave = enemiesInWave $ head (waves _currentLevel)

    removeWaveAfterSpawn :: [Wave]
    removeWaveAfterSpawn = tail $ waves _currentLevel

-- jsonFile :: FilePath
-- jsonFile = "level.json"
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

-- getLevels :: IO [Level]
getLevelsInJSON :: IO [LevelJSON]
getLevelsInJSON = do
  levelFileNames <- listDirectory levelsPath
  mapM loadLevel levelFileNames

myLevel :: LevelJSON
myLevel = LevelJSON 1 [WaveJSON [AirplaneJSON Fighter (300, -300), AirplaneJSON Fighter (300, 300)] 50, WaveJSON [AirplaneJSON Kamikaze (300, -300), AirplaneJSON Kamikaze (300, 300)] 50]

myAirplaneJSON :: AirplaneJSON
myAirplaneJSON = AirplaneJSON Fighter (0, 0)

writeJSONLevelToJson :: FilePath -> IO ()
writeJSONLevelToJson filePath = do
  B.writeFile filePath (encode myLevel)

debugInitLevel :: Assets -> Level
debugInitLevel assetlist = Level {levelNr = 1, waves = [Wave [createEnemy Fighter (500, -350) assetlist, createEnemy Fighter (300, -200) assetlist] 200, Wave [createEnemy Fighter (500, 350) assetlist] 200, Wave [createEnemy Kamikaze (300, 100) assetlist] 200]}

levelConverter :: LevelJSON -> Assets -> Level
levelConverter LevelJSON {resLevelNr = _resLevelNr, resWaves = _resWaves} assetList =
  Level {levelNr = _resLevelNr, waves = getWaves _resWaves}
  where
    getWaves :: [WaveJSON] -> [Wave]
    getWaves [] = []
    getWaves (x : xs) = waveConverter x assetList : getWaves xs

waveConverter :: WaveJSON -> Assets -> Wave
waveConverter WaveJSON {resEnemiesInWave = _resEnemiesInWave, resWaveTimer = _resWaveTimer} assetList =
  Wave {enemiesInWave = getEnemies _resEnemiesInWave, waveTimer = _resWaveTimer}
  where
    getEnemies :: [AirplaneJSON] -> [Airplane]
    getEnemies [] = []
    getEnemies (x : xs) = airplaneConverter x assetList : getEnemies xs

airplaneConverter :: AirplaneJSON -> Assets -> Airplane
airplaneConverter AirplaneJSON {resAirplaneType = _resAirplaneType, resAirplanePos = _resAirplanePos} = createEnemy _resAirplaneType (_resAirplanePos + (C.screenMaxX, 0))

createEnemy :: AirPlaneType -> Position -> Assets -> Enemy
createEnemy airplaneType' airplanePosition' assetList = case airplaneType' of
  Fighter ->
    createAirplaneBase
      { airplaneMaxVelocity = (-12, 12),
        airplaneHealth = 100,
        fireRate = Single 120.0,
        airplaneGun = createSingleGun,
        airplaneSprite = createAirplaneSprite
      }
  Kamikaze ->
    createAirplaneBase
      { airplaneMaxVelocity = (-12, 12),
        airplaneHealth = 100,
        fireRate = Single 999999.0,
        airplaneGun = None,
        airplaneSprite = createAirplaneSprite
      }
  FlyBy ->
    createAirplaneBase
      { airplaneMaxVelocity = (-12, 12),
        airplaneHealth = 100,
        fireRate = Burst 120.0,
        airplaneGun = createSingleGun,
        airplaneSprite = createAirplaneSprite
      }
  Player1 -> error "Creating a player" -- TODO What do we need to do in this situation?
  Player2 -> error "Creating a player"
  where
    createAirplaneBase :: Airplane
    createAirplaneBase =
      Airplane
        { airplaneType = airplaneType',
          airplanePos = airplanePosition',
          airplaneDestinationPos = (0, 0),
          airplaneSize = C.airplaneSizeVar,
          airplaneVelocity = (0, 0),
          timeLastShot = 0.0,
          airplanePowerUps = []
        }

    createSingleGun :: AirplaneGun
    createSingleGun =
      AirplaneGun
        Projectile
          { projectileType = Gun,
            projectilePos = (0, 0),
            projectileSize = C.projectileSizeVar,
            projectileVelocity = (-10, 0),
            projectileHealth = 1,
            projectileDamage = 10,
            projectileOrigin = Enemies,
            projectileSprite = flip fixImageOrigin C.projectileSizeVar $ rotate (-90) $ getTexture "bullet" assetList
          }

    createAirplaneSprite :: Picture
    createAirplaneSprite = case airplaneType' of
      Kamikaze -> flip fixImageOrigin C.airplaneSizeVar $ getTexture (show airplaneType') assetList
      _ -> flip fixImageOrigin C.airplaneSizeVar $ rotate (-90) $ getTexture (show airplaneType') assetList

-- ! remove to own file
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