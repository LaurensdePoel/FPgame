module Level where

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