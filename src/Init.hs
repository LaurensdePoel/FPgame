module Init where

import Assets (errorSprite, getParticle, getTexture)
-- import Graphics.Gloss (Picture (Scale))

import Config as C
import Data.Map as Dict (fromList)
import Graphics.Gloss
import Input
import Level
import Menu
import Model

-- | creates an empty level which should never be displayed.
initEmptyLevel :: Level
initEmptyLevel = Level 0 (Background (0, 0) (errorSprite "emptyLevel")) []

-- | reset the IOActions record
emptyIOActions :: IOActions
emptyIOActions = IOActions False False

-- | this function creates the initial 'GameState' that will be used when the game is run for the first time.
-- It loads all the nessesery records so that they are available to all the other parts of the game.
initialState :: Assets -> [Level] -> Menu -> GameState
initialState assetlist levelList levelSelectMenu' =
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
      menu = initMenu assetlist,
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
                  particleSprites = [getTexture "explotion_1" assetlist, getTexture "explotion_2" assetlist, getTexture "explotion_3" assetlist, getTexture "explotion_4" assetlist, getTexture "explotion_5" assetlist]
                }
            ),
            ( "explosion2",
              Particle
                { particlePos = (0, 0),
                  particleSize = (0, 0),
                  particleInterval = 8,
                  particleTimer = 8,
                  particleSprites = [Scale 2.0 2.0 $ getTexture "explotion_1" assetlist, Scale 2.0 2.0 $ getTexture "explotion_2" assetlist, Scale 2.0 2.0 $ getTexture "explotion_3" assetlist, Scale 2.0 2.0 $ getTexture "explotion_4" assetlist, Scale 2.0 2.0 $ getTexture "explotion_5" assetlist]
                }
            ),
            ( "5SecondTimer",
              Particle
                { particlePos = (-400, 80),
                  particleSize = (10, 10),
                  particleInterval = 60,
                  particleTimer = 60,
                  particleSprites = [getTexture "5" assetlist, getTexture "4" assetlist, getTexture "3" assetlist, getTexture "2" assetlist, getTexture "1" assetlist]
                }
            ),
            ( "level1",
              C.defaultTextParticle
                { particlePos = (0, 300),
                  particleInterval = 30,
                  particleTimer = 30,
                  particleSprites = [getTexture "text_level1" assetlist, getTexture "text2_level1" assetlist, getTexture "text_level1" assetlist, getTexture "text2_level1" assetlist]
                }
            ),
            ( "level2",
              C.defaultTextParticle
                { particlePos = (0, 300),
                  particleInterval = 30,
                  particleTimer = 30,
                  particleSprites = [getTexture "text_level2" assetlist, getTexture "text2_level2" assetlist, getTexture "text_level2" assetlist, getTexture "text2_level2" assetlist]
                }
            ),
            ( "level3",
              C.defaultTextParticle
                { particlePos = (0, 300),
                  particleInterval = 30,
                  particleTimer = 30,
                  particleSprites = [getTexture "text_level3" assetlist, getTexture "text2_level3" assetlist, getTexture "text_level3" assetlist, getTexture "text2_level3" assetlist]
                }
            ),
            ( "level4",
              C.defaultTextParticle
                { particlePos = (0, 300),
                  particleInterval = 30,
                  particleTimer = 30,
                  particleSprites = [getTexture "text_level4" assetlist, getTexture "text2_level4" assetlist, getTexture "text_level4" assetlist, getTexture "text2_level4" assetlist]
                }
            ),
            ( "NextWave",
              C.defaultTextParticle
                { particlePos = (300, 0),
                  particleInterval = 10,
                  particleTimer = 10,
                  particleSprites = [getTexture "text_nextwave" assetlist, getTexture "text2_nextwave" assetlist, getTexture "text_nextwave" assetlist, getTexture "text2_nextwave" assetlist, getTexture "text_nextwave" assetlist, getTexture "text2_nextwave" assetlist]
                }
            )
          ],
      tmpassetList = assetlist,
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
      -- status = _status,
      -- nrOfPlayers = 0
      players = [],
      enemies = [],
      -- levels = _levels,
      -- currentLevel = initEmptyLevel,
      -- currentLevelNr = 0,
      projectiles = [],
      powerUps = [],
      -- particleMap = _particleMap,
      particles = [],
      pressedKeys = emptyKeys -- TODO Check if this isn't done multible times
      -- menu = _menu,
      -- levelSelectMenu = _levelSelectMenu,
      -- tmpassetList = _tmpassetList
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

-- TODO Make higher order function
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
loadPlayers gs@GameState {tmpassetList = _assets, nrOfPlayers = _nrOfPlayers} = gs {players = addPlayers _assets _nrOfPlayers}

startLevel :: GameState -> GameState
startLevel gs@GameState {tmpassetList = _assetList, menu = _menu, particles = _particles, particleMap = _particleMap, currentLevel = _currentLevel} =
  gs
    { status = InGame,
      menu = initPauseMenu _assetList,
      particles = getParticle ("level" ++ show (levelNr _currentLevel)) _particleMap : _particles
    }

-- Toggles the status in the GameState.
startFromLevelSelect :: GameState -> GameState
startFromLevelSelect gs =
  startLevel $ loadPlayers $ getLevelFromMenu $ resetGameState gs
  where
    getLevelFromMenu :: GameState -> GameState
    getLevelFromMenu gss@GameState {tmpassetList = _assets, menu = _menu, levels = _levels, players = _players, nrOfPlayers = _nrOfPlayers} =
      gss
        { currentLevel = _levels !! getLevelIndex _menu,
          currentLevelNr = getLevelIndex _menu
        }

exitGame :: GameState -> GameState
exitGame gs = gs {ioActions = IOActions False True}