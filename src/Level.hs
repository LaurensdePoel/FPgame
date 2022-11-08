{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Level where

import Assets (fixImageOrigin, getTexture)
import qualified Config as C
import Control.Applicative ()
import Control.Monad ()
import Data.Aeson ()
import Data.List ()
import GHC.Generics ()
import Graphics.Gloss.Data.Picture (Picture, rotate)
import LoadLevels (AirplaneJSON (..), LevelJSON (..), WaveJSON (..))
import Model
import System.Directory ()

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

-- TODO REFACTOR use readMaybe
getLevelIndex :: Menu -> Int
getLevelIndex menu' = read (fieldName $ head $ fields menu') - 1

levelConverter :: LevelJSON -> Assets -> Level
levelConverter LevelJSON {resLevelNr = _resLevelNr, resWaves = _resWaves} assetList =
  Level {levelNr = _resLevelNr, waves = convertWaves _resWaves}
  where
    convertWaves :: [WaveJSON] -> [Wave]
    convertWaves = map (`waveConverter` assetList)

waveConverter :: WaveJSON -> Assets -> Wave
waveConverter WaveJSON {resEnemiesInWave = _resEnemiesInWave, resWaveTimer = _resWaveTimer} assetList =
  Wave {enemiesInWave = convertEnemies _resEnemiesInWave, waveTimer = _resWaveTimer}
  where
    convertEnemies :: [AirplaneJSON] -> [Airplane]
    convertEnemies = map (`airplaneConverter` assetList)

addPlayers :: Assets -> Int -> [Airplane]
addPlayers assets amount
  | amount == 1 = addPlayer assets []
  | amount == 2 = addPlayer assets $ addPlayer assets []
  | otherwise = []

addPlayer :: Assets -> [Airplane] -> [Airplane]
addPlayer assets xs
  | null xs = xs ++ [createAirplane Player1 (-300, 100) assets]
  | length xs == 1 = xs ++ [createAirplane Player2 (-300, -100) assets]
  | otherwise = xs

-- | creates a enemy airplane based on the Type and set the spawning location. The spawning position is determend by the (absolute x position + screenWidth) and the (y position in the JSON file)
airplaneConverter :: AirplaneJSON -> Assets -> Airplane
airplaneConverter AirplaneJSON {resAirplaneType = _resAirplaneType, resAirplanePos = (airplaneX, airplaneY)} = createAirplane _resAirplaneType ((abs airplaneX, airplaneY) + (C.screenMaxX, 0))

createAirplane :: AirPlaneType -> Position -> Assets -> Enemy
createAirplane airplaneType' airplanePosition' assetList = case airplaneType' of
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
        fireRate = Burst 30.0,
        airplaneGun = createSingleGun,
        airplaneSprite = createAirplaneSprite
      }
  Player1 ->
    createAirplaneBase
      { airplaneMaxVelocity = (-12, 12),
        airplaneHealth = 100,
        fireRate = Single 80.0,
        airplaneGun = createSingleGun,
        airplaneSprite = createAirplaneSprite
      }
  Player2 ->
    createAirplaneBase
      { airplaneMaxVelocity = (-12, 12),
        airplaneHealth = 100,
        fireRate = Single 80.0,
        airplaneGun = createSingleGun,
        airplaneSprite = createAirplaneSprite
      }
  where
    createAirplaneBase :: Airplane
    createAirplaneBase =
      Airplane
        { airplaneType = airplaneType',
          airplanePos = airplanePosition',
          airplaneDestinationPos = (C.screenMaxX, snd airplanePosition'),
          airplaneSize = C.airplaneSizeVar,
          airplaneVelocity = (0, 0),
          timeLastShot = 0.0,
          airplanePowerUps = []
        }

    createSingleGun :: AirplaneGun
    createSingleGun = case airplaneType' of
      Player1 ->
        AirplaneGun
          Projectile
            { projectileType = Gun,
              projectilePos = (0, 0),
              projectileSize = C.projectileSizeVar,
              projectileVelocity = (10, 0),
              projectileHealth = 1,
              projectileDamage = 10,
              projectileOrigin = Players,
              projectileSprite = flip fixImageOrigin C.projectileSizeVar $ rotate (-90) $ getTexture "bullet" assetList
            }
      Player2 ->
        AirplaneGun
          Projectile
            { projectileType = Gun,
              projectilePos = (0, 0),
              projectileSize = C.projectileSizeVar,
              projectileVelocity = (10, 0),
              projectileHealth = 1,
              projectileDamage = 10,
              projectileOrigin = Players,
              projectileSprite = flip fixImageOrigin C.projectileSizeVar $ rotate (-90) $ getTexture "bullet" assetList
            }
      _ ->
        AirplaneGun
          Projectile
            { projectileType = Gun,
              projectilePos = (0, 0),
              projectileSize = C.projectileSizeVar,
              projectileVelocity = (-40, 0),
              projectileHealth = 1,
              projectileDamage = 10,
              projectileOrigin = Enemies,
              projectileSprite = flip fixImageOrigin C.projectileSizeVar $ rotate (-90) $ getTexture "bullet" assetList
            }

    createAirplaneSprite :: Picture
    createAirplaneSprite = case airplaneType' of
      Kamikaze -> flip fixImageOrigin C.airplaneSizeVar $ getTexture (show airplaneType') assetList
      Player1 -> flip fixImageOrigin C.airplaneSizeVar $ rotate 90 $ getTexture "player_1" assetList
      Player2 -> flip fixImageOrigin C.airplaneSizeVar $ rotate 90 $ getTexture "player_2" assetList
      _ -> flip fixImageOrigin C.airplaneSizeVar $ rotate (-90) $ getTexture (show airplaneType') assetList
