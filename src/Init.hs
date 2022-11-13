module Init where

import Assets (errorSprite, getParticle, getTexture)
-- import Graphics.Gloss (Picture (Scale))

import Config as C
import Data.Map as Dict (fromList)
import Graphics.Gloss (Picture (Scale))
import Input (emptyKeys)
import Level (addPlayers, getLevelIndex)
import Menu (createMenu)
import Model

-- | creates an empty level which should never be displayed.
initEmptyLevel :: Level
initEmptyLevel = Level 0 (Background (0, 0) (errorSprite "emptyLevel")) []

-- | reset the IOActions record
emptyIOActions :: IOActions
emptyIOActions = IOActions False False

-- | this function creates the initial 'GameState' that will be used when the game is run for the first time.
-- It loads all the necessary records so that they are available to all the other parts of the game.
initialState :: Assets -> [Level] -> Menu -> GameState
initialState assets levelList levelSelectMenu' =
  GameState
    { elapsedTime = 0,
      status = InMenu,
      players = [],
      nrOfPlayers = 0,
      enemies = [],
      levels = levelList,
      currentLevel = initEmptyLevel,
      currentLevelNr = 0,
      projectiles = [],
      powerUps = [],
      pressedKeys = emptyKeys,
      menu = initMenu assets,
      levelSelectMenu = levelSelectMenu',
      particles = [],
      particleMap =
        Dict.fromList
          [ ( "explosion",
              Particle
                { particlePos = (0, 0),
                  particleSize = (0, 0),
                  particleInterval = 8,
                  particleTimer = 8,
                  particleSprites = [getTexture "explotion_1" assets, getTexture "explotion_2" assets, getTexture "explotion_3" assets, getTexture "explotion_4" assets, getTexture "explotion_5" assets]
                }
            ),
            ( "explosion2",
              Particle
                { particlePos = (0, 0),
                  particleSize = (0, 0),
                  particleInterval = 8,
                  particleTimer = 8,
                  particleSprites = [Scale 2.0 2.0 $ getTexture "explotion_1" assets, Scale 2.0 2.0 $ getTexture "explotion_2" assets, Scale 2.0 2.0 $ getTexture "explotion_3" assets, Scale 2.0 2.0 $ getTexture "explotion_4" assets, Scale 2.0 2.0 $ getTexture "explotion_5" assets]
                }
            ),
            ( "powerUpTimer",
              Particle
                { particlePos = (-400, 80),
                  particleSize = (10, 10),
                  particleInterval = C.powerUpDespawnTime * 0.2,
                  particleTimer = C.powerUpDespawnTime * 0.2,
                  particleSprites = [getTexture "5" assets, getTexture "4" assets, getTexture "3" assets, getTexture "2" assets, getTexture "1" assets]
                }
            ),
            ( "level1",
              C.defaultTextParticle
                { particlePos = (0, 300),
                  particleSprites = [getTexture "text_level1" assets]
                }
            ),
            ( "level2",
              C.defaultTextParticle
                { particlePos = (0, 300),
                  particleSprites = [getTexture "text_level2" assets]
                }
            ),
            ( "level3",
              C.defaultTextParticle
                { particlePos = (0, 300),
                  particleSprites = [getTexture "text_level3" assets]
                }
            ),
            ( "level4",
              C.defaultTextParticle
                { particlePos = (0, 300),
                  particleSprites = [getTexture "text_level4" assets]
                }
            ),
            ( "NextWave",
              C.defaultTextParticle
                { particlePos = (300, 0),
                  particleSprites = [getTexture "text_nextwave" assets]
                }
            )
          ],
      assetMap = assets,
      ioActions = emptyIOActions
    }

resetGameState :: GameState -> GameState
resetGameState gs =
  resetLevel
    gs
      { currentLevel = initEmptyLevel,
        currentLevelNr = 0
      }

resetLevel :: GameState -> GameState
resetLevel gs =
  gs
    { elapsedTime = 0,
      players = [],
      enemies = [],
      projectiles = [],
      powerUps = [],
      particles = [],
      pressedKeys = emptyKeys -- TODO Check if this isn't done multible times
    }

-- | Create menu's
initMenu, initPlayMenu, initPauseMenu, initCreditMenu, initControlsMenu :: Assets -> Menu
initMenu assets = createMenu "Shoot'em Up" (getTexture "menu" assets) NoMenu [("Play", initPlayMenu assets), ("Controls", initControlsMenu assets), ("Credits", initCreditMenu assets), ("Exit", NoMenuButFunction exitGame)]
initPlayMenu assets = createMenu "Choose players" (getTexture "menu" assets) (initMenu assets) [("1 Player", NoMenuButFunction loadLevelselectAnd1Player), ("2 Players", NoMenuButFunction loadLevelselectAnd2Player)]
initPauseMenu assets = createMenu "Paused" (getTexture "menu" assets) NoMenu [("Resume", NoMenuButFunction resumeGame), ("Return to menu", initMenu assets)]
initCreditMenu assets = createMenu "Credits" (getTexture "credits" assets) (initMenu assets) [("", NoMenu)]
initControlsMenu assets = createMenu "Controls" (getTexture "controls" assets) (initMenu assets) [("", NoMenu)]

initVictoryMenu :: Assets -> Menu
initVictoryMenu assets = createMenu "Level Completed" (getTexture "menu" assets) NoMenu [("Next Level", NoMenuButFunction nextLevel), ("Select Level", NoMenuButFunction loadLevelSelectMenu), ("Return to Menu", initMenu assets)] -- TODO: NoMenuButFunction start1player is incorrect

initDefeatMenu :: Assets -> Menu
initDefeatMenu assets = createMenu "Game Over" (getTexture "menu" assets) NoMenu [("Retry Level", NoMenuButFunction retryLevel), ("Select Level", NoMenuButFunction loadLevelSelectMenu), ("Return to Menu", initMenu assets)] -- TODO: NoMenuButFunction start1player is incorrect

createLevelSelectmenu :: [Level] -> Assets -> Menu
createLevelSelectmenu levelList assets = createMenu "Level Select" (getTexture "menu" assets) (initPlayMenu assets) (reloadLevelsField : createLevelFields levelList)
  where
    createLevelFields :: [Level] -> [(String, Menu)]
    createLevelFields [] = []
    createLevelFields (x : xs)
      | levelNr x == -1 = ("Error with JSON", NoMenu) : createLevelFields xs
      | otherwise = (show (levelNr x), NoMenuButFunction startFromLevelSelect) : createLevelFields xs

    reloadLevelsField :: (String, Menu)
    reloadLevelsField = ("Reload Levels", NoMenuButFunction reloadLevels)

reloadLevels :: GameState -> GameState
reloadLevels gs = gs {ioActions = IOActions True False}

resumeGame :: GameState -> GameState
resumeGame gs = gs {status = InGame}

retryLevel :: GameState -> GameState
retryLevel gs@GameState {levels = _levels, currentLevel = _level} = startLevel $ loadPlayers $ resetLevel $ loadLevel gs

loadLevelSelectMenu :: GameState -> GameState
loadLevelSelectMenu gs@GameState {levelSelectMenu = _levelSelectMenu} = gs {menu = _levelSelectMenu}

loadLevelselectAnd1Player :: GameState -> GameState
loadLevelselectAnd1Player gs@GameState {nrOfPlayers = _nrOfPlayers} = loadLevelSelectMenu gs {nrOfPlayers = 1}

loadLevelselectAnd2Player :: GameState -> GameState
loadLevelselectAnd2Player gs@GameState {nrOfPlayers = _nrOfPlayers} = loadLevelSelectMenu gs {nrOfPlayers = 2}

nextLevel :: GameState -> GameState
nextLevel gs@GameState {levels = _levels, currentLevel = _level}
  | levelNr _level == length _levels = loadLevelSelectMenu gs
  | otherwise = startLevel $ loadPlayers $ resetLevel $ loadLevel gs {currentLevelNr = levelNr _level}

loadLevel :: GameState -> GameState
loadLevel gs@GameState {levels = _levels, currentLevelNr = _currentLevelNr, menu = _menu} =
  gs
    { currentLevel = _levels !! _currentLevelNr
    }

loadPlayers :: GameState -> GameState
loadPlayers gs@GameState {assetMap = _assets, nrOfPlayers = _nrOfPlayers} = gs {players = addPlayers _assets _nrOfPlayers}

startLevel :: GameState -> GameState
startLevel gs@GameState {assetMap = _assets, menu = _menu, particles = _particles, particleMap = _particleMap, currentLevel = _currentLevel} =
  gs
    { status = InGame,
      menu = initPauseMenu _assets,
      particles = getParticle ("level" ++ show (levelNr _currentLevel)) _particleMap : _particles
    }

-- Toggles the status in the GameState.
startFromLevelSelect :: GameState -> GameState
startFromLevelSelect gs =
  startLevel $ loadPlayers $ getLevelFromMenu $ resetGameState gs
  where
    getLevelFromMenu :: GameState -> GameState
    getLevelFromMenu gss@GameState {assetMap = _assets, menu = _menu, levels = _levels, players = _players, nrOfPlayers = _nrOfPlayers} =
      gss
        { currentLevel = _levels !! getLevelIndex _menu,
          currentLevelNr = getLevelIndex _menu
        }

exitGame :: GameState -> GameState
exitGame gs = gs {ioActions = IOActions False True}