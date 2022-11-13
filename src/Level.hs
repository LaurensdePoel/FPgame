{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Level where

import Assets (fixImageOrigin, getParticle, getTexture)
import Config as C
import Control.Applicative ()
import Control.Monad ()
import Data.Aeson ()
import Data.List ()
import GHC.Generics ()
import Graphics.Gloss (Picture, pictures, rotate, translate)
import LoadLevels (AirplaneJSON (..), LevelJSON (..), WaveJSON (..))
import Model
import System.Directory ()
import Text.Read (readMaybe)

-- | The function 'nextwave' sets the nextwave as currentwave. If there are no more waves this functions does nothing.
nextWave :: GameState -> GameState
nextWave gs@GameState {currentLevel = _currentLevel, enemies = _enemies, particles = _particles, particleMap = _particleMap}
  | ifAllWavesCleared = gs
  | otherwise =
      gs
        { enemies = _enemies ++ spawnNextWave,
          currentLevel = _currentLevel {waves = removeWaveAfterSpawn},
          particles = waveParticles : _particles
        }
  where
    ifAllWavesCleared :: Bool
    ifAllWavesCleared = null (waves _currentLevel)

    spawnNextWave :: [Enemy]
    spawnNextWave = enemiesInWave $ head (waves _currentLevel)

    removeWaveAfterSpawn :: [Wave]
    removeWaveAfterSpawn = tail $ waves _currentLevel

    waveParticles :: Particle
    waveParticles = getParticle "NextWave" _particleMap

-- | This function tries to convert the name of the selected menu. In case it is a Level it is a number.
getLevelIndex :: Menu -> Int
getLevelIndex menu' = case readMaybe (fieldName $ head $ fields menu') of
  Nothing -> 0 -- fault when reading level index so start level 1
  Just x -> x - 1 -- from number to index in list -> -1

-- | Converts a 'LevelJSON' to a 'Level' record wich will be used by the game.
levelConverter :: LevelJSON -> Assets -> Level
levelConverter LevelJSON {resLevelNr = _resLevelNr, resLevelBackgroundName = _backgroundName, resWaves = _resWaves} assets =
  Level {levelNr = _resLevelNr, levelBackground = background, waves = convertWaves _resWaves}
  where
    convertWaves :: [WaveJSON] -> [Wave]
    convertWaves = map (`waveConverter` assets)

    background =
      Background
        (0, 0)
        ( pictures
            [ getTexture _backgroundName assets,
              translate (fromIntegral C.screenWidth) 0 $ getTexture _backgroundName assets
            ]
        )

-- | Converts a 'WaveJSON' to a 'Wave' record wich will be used by to spawn enemies in the level.
waveConverter :: WaveJSON -> Assets -> Wave
waveConverter WaveJSON {resEnemiesInWave = _resEnemiesInWave, resWaveTimer = _resWaveTimer} assets =
  Wave {enemiesInWave = convertEnemies _resEnemiesInWave, waveTimer = _resWaveTimer}
  where
    convertEnemies :: [AirplaneJSON] -> [Airplane]
    convertEnemies = map (`airplaneConverter` assets)

-- | based on the number retruns 1 or 2 players.
addPlayers :: Assets -> Int -> [Airplane]
addPlayers assets amount
  | amount == 1 = addPlayer assets []
  | amount == 2 = addPlayer assets $ addPlayer assets []
  | otherwise = []

-- | add a player to the given airplane list if there are not already 2 players in the list.
addPlayer :: Assets -> [Airplane] -> [Airplane]
addPlayer assets xs
  | null xs = xs ++ [createAirplane Player1 C.player1SpawnLocation assets]
  | length xs == 1 = xs ++ [createAirplane Player2 C.player2SpawnLocation assets]
  | otherwise = xs

-- | creates a enemy airplane based on the Type and set the spawning location.
-- The spawning position is determent by the (absolute x position + screenWidth) and the (y position in the JSON file)
airplaneConverter :: AirplaneJSON -> Assets -> Airplane
airplaneConverter AirplaneJSON {resAirplaneType = _resAirplaneType, resAirplanePos = (airplaneX, airplaneY)} =
  createAirplane _resAirplaneType ((abs airplaneX, airplaneY) + (C.screenMaxX, 0))

-- | constructs a airplane based on some passed parameters.
createAirplane :: AirPlaneType -> Position -> Assets -> Airplane
createAirplane airplaneType' airplanePosition' assets' =
  case airplaneType' of
    Fighter -> C.fighterAirplane airplaneType' airplanePosition' assets' createAirplaneSprite
    Kamikaze -> C.kamikazeAirplane airplaneType' airplanePosition' createAirplaneSprite
    FlyBy -> C.flybyAirplane airplaneType' airplanePosition' assets' createAirplaneSprite
    Player1 -> C.playerAirplane airplaneType' airplanePosition' assets' createAirplaneSprite
    Player2 -> C.playerAirplane airplaneType' airplanePosition' assets' createAirplaneSprite
  where
    -- \| Creates the airplane sprite based on the airplane type
    createAirplaneSprite :: Picture
    createAirplaneSprite = case airplaneType' of
      Kamikaze -> flip fixImageOrigin C.airplaneSizeVar $ getTexture (show airplaneType') assets'
      Player1 -> flip fixImageOrigin C.airplaneSizeVar $ rotate C.playerSpriteRotation $ getTexture "player_1" assets'
      Player2 -> flip fixImageOrigin C.airplaneSizeVar $ rotate C.playerSpriteRotation $ getTexture "player_2" assets'
      _ -> flip fixImageOrigin C.airplaneSizeVar $ rotate C.enemySpriteRotation $ getTexture (show airplaneType') assets'