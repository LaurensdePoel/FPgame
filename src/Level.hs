{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Level where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Model

-- | The function 'nextwave' sets the nextwave as currentwave. If there are no more waves this functions does nothing.
nextWave :: GameState -> GameState
nextWave gs@GameState {levels = _levels, enemies = _enemies}
  | ifAllWavesCleared = gs -- do nothing if all waves are cleared --TODO better if we can disable timer
  | otherwise = gs {enemies = _enemies ++ spawnNextWave, levels = _levels {waves = removeWaveAfterSpawn}}
  where
    ifAllWavesCleared :: Bool
    ifAllWavesCleared = null (waves _levels)

    spawnNextWave :: [Enemy]
    spawnNextWave = enemiesInWave $ head (waves _levels)

    removeWaveAfterSpawn :: [Wave]
    removeWaveAfterSpawn = tail $ waves _levels

jsonFile :: FilePath
jsonFile = "level.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

myLevel :: ResLevel
myLevel = ResLevel 1 [ResWave [ResAirplane Fighter (300, -300), ResAirplane Fighter (300, 300)] 50, ResWave [ResAirplane Kamikaze (300, -300), ResAirplane Kamikaze (300, 300)] 50]

data Foo = Foo
  { field1 :: Int,
    field2 :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

myFoo :: Foo
myFoo =
  Foo
    { field1 = 909,
      field2 = "take your time"
    }

-- ToJSON so that we can encode *to* a JSON string,
-- FromJSON so that we can parse *from* a JSON string

-- ! remove to own file
data ResLevel = ResLevel
  { resLevelNr :: Int,
    resWaves :: [ResWave]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ResWave = ResWave
  { resEnemiesInWave :: [ResAirplane],
    resWaveTimer :: Time
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ResAirplane = ResAirplane
  { resAirplaneType :: AirPlaneType,
    resAirplanePos :: Position
  }
  deriving (Show, Generic, ToJSON, FromJSON)
