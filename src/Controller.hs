-- | This module defines the global gameloop
module Controller where

import Config as C
import Enemy
import Graphics.Gloss.Interface.IO.Game
import Handler
import Input
import Level
import Menu
import Model
import Random
import System.Random

step :: Float -> GameState -> IO GameState
step seconds gs@GameState {status = _status}
  | _status == QuitGame = do return gs
  | otherwise = do
      gen <- newStdGen -- getStdGen
      return $ stepPure seconds gen gs

-- | Handle one iteration of the game
--
-- if status = InMenu -> updateMenu
-- if status = InGame -> updateGameState
stepPure :: Float -> StdGen -> GameState -> GameState
stepPure seconds gen gs@GameState {status = _status}
  | _status == InMenu = updateMenu gs
  | _status == InGame = updateGameState randomPoint $ gs {elapsedTime = updateTime, powerUps = updatedPowerUpList}
  | otherwise = gs
  where
    updateTime :: Time
    updateTime = elapsedTime gs + seconds
    randomPoint = getRandomPoint C.enemyXBounds C.enemyYBounds gen -- mkStdGen $ round updateTime
    updatedPowerUpList = case spawnPowerUp gen (tmpassetList gs) of
      Just x -> x : powerUps gs
      Nothing -> powerUps gs

-- | GameLoop while game is in state InMenu
updateMenu :: GameState -> GameState
updateMenu = checkMenuInput

-- | GameLoop while game is in state InGame
updateGameState :: Position -> GameState -> GameState
updateGameState randomPoint = debugButtons . checkPause . levelHandler . garbageCollector . particleHandler . collisionHandler . timeHandler . movementHandler . enemyBehaviourHandler randomPoint

-- | When ESC key is pressed change the Status to InMenu
checkPause :: GameState -> GameState
checkPause gs = singleKeyPress (SpecialKey KeyEsc) gs pauseMenu

-- | Define cheat/debug keys used by the programmers
--
-- Currently defined keys:
--
--    - 'f' = Force next wave
--    - 'k' = Kill oldest enemy
debugButtons :: GameState -> GameState
debugButtons = debugSpawnButton . debugKillEnemyButton
  where
    -- Force next wave
    debugSpawnButton :: GameState -> GameState
    debugSpawnButton gs@GameState {pressedKeys = _pressedKeys} = singleKeyPress (Char 'f') gs nextWave

    -- Kill oldest enemy
    debugKillEnemyButton :: GameState -> GameState
    debugKillEnemyButton gs@GameState {pressedKeys = _pressedKeys} = singleKeyPress (Char 'k') gs killTopEnemy
      where
        killTopEnemy gs'@GameState {enemies = _enemies}
          | null _enemies = gs'
          | otherwise = gs' {enemies = tail _enemies}
