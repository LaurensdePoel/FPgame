module Init where

import Assets (fixImageOrigin, getTexture)
import Config (airplaneSizeVar, projectileSizeVar)
import Data.Map as Dict (fromList)
import Graphics.Gloss (Picture (Scale), rotate)
import Input
import Level
import Menu
import Model

initEmptyLevel :: Level
initEmptyLevel = Level 0 []

initialState :: Assets -> [Level] -> Menu -> GameState
initialState assetlist levelList levelSelectMenu' =
  GameState
    { elapsedTime = 0,
      status = InMenu,
      players = [],
      enemies = [],
      levels = levelList,
      currentLevel = initEmptyLevel,
      currentLevelNr = 0,
      projectiles = [],
      powerUps = [],
      pressedKeys = emptyKeys,
      menu = initMenu,
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
            )
          ],
      tmpassetList = assetlist
    }

resetGameState :: GameState -> GameState
resetGameState gs =
  gs
    { elapsedTime = 0,
      -- status = _status,
      players = [],
      enemies = [],
      -- levels = _levels,
      currentLevel = initEmptyLevel,
      currentLevelNr = 0,
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
initMenu, initPlayMenu, initPauseMenu :: Menu
initMenu = createMenu "Shoot'em Up" NoMenu [("Play", initPlayMenu), ("Controls", NoMenu), ("Credits", NoMenu), ("Exit", NoMenu)]
-- initPlayMenu = createMenu "Choose players" initMenu [("1 Player", NoMenuButFunction start1player), ("2 Player", NoMenuButFunction start2player)]
initPlayMenu = createMenu "Choose players" initMenu [("1 Player", NoMenuButFunction loadLevelSelectMenu), ("2 Player", NoMenuButFunction loadLevelSelectMenu)]
initPauseMenu = createMenu "Paused" NoMenu [("Resume", NoMenuButFunction resumeGame), ("Return to menu", initMenu)]

initVictoryMenu :: Menu
initVictoryMenu = createMenu "Level Completed" NoMenu [("Next Level", NoMenuButFunction nextLevel), ("Select Level", NoMenuButFunction loadLevelSelectMenu), ("Return to Menu", initMenu)] -- TODO: NoMenuButFunction start1player is incorrect

initDefeatMenu :: Menu
initDefeatMenu = createMenu "Game Over" NoMenu [("Retry Level", NoMenuButFunction loadLevelSelectMenu), ("Select Level", NoMenuButFunction loadLevelSelectMenu), ("Return to Menu", initMenu)] -- TODO: NoMenuButFunction start1player is incorrect

-- TODO Make higher order function
createLevelSelectmenu :: [Level] -> Menu
createLevelSelectmenu levelList = createMenu "Level Select" initPlayMenu $ createLevelFields levelList
  where
    createLevelFields :: [Level] -> [(String, Menu)]
    createLevelFields [] = []
    createLevelFields (x : xs) = (show (levelNr x), NoMenuButFunction start1player) : createLevelFields xs

resumeGame :: GameState -> GameState
resumeGame gs = gs {status = InGame}

retryLevel :: GameState -> GameState -- TODO refactor so game level can startup again with healed players.
retryLevel gs@GameState {levels = _levels, currentLevel = _level} = startLevel $ loadLevel gs

loadLevelSelectMenu :: GameState -> GameState
loadLevelSelectMenu gs@GameState {levelSelectMenu = _levelSelectMenu} = gs {menu = _levelSelectMenu}

nextLevel :: GameState -> GameState
nextLevel gs@GameState {levels = _levels, currentLevel = _level}
  | levelNr _level == length _levels = loadLevelSelectMenu gs
  | otherwise = startLevel $ loadLevel gs {currentLevelNr = levelNr _level + 1}

loadLevel :: GameState -> GameState
loadLevel gs@GameState {levels = _levels, currentLevelNr = _currentLevelNr, menu = _menu} =
  gs
    { currentLevel = _levels !! (_currentLevelNr -1)
    }

startLevel :: GameState -> GameState
startLevel gs@GameState {tmpassetList = _assetList, menu = _menu} =
  gs
    { status = InGame,
      menu = initPauseMenu
    }

-- Toggles the status in the GameState.
start1player :: GameState -> GameState -- TODO make start function and load1Player or load2Player
start1player gs =
  startAndLoad1Player $ resetGameState gs
  where
    startAndLoad1Player :: GameState -> GameState
    startAndLoad1Player gss@GameState {tmpassetList = _assetList, menu = _menu, levels = _levels} =
      gss
        { players =
            [ Airplane
                { airplaneType = Player1,
                  airplanePos = (-400, 0),
                  airplaneDestinationPos = (0, 0),
                  airplaneSize = airplaneSizeVar,
                  airplaneVelocity = (0, 0),
                  airplaneMaxVelocity = (-12, 12),
                  airplaneHealth = 100,
                  fireRate = Single 30.0,
                  timeLastShot = 0.0,
                  airplanePowerUps = [],
                  airplaneGun =
                    AirplaneGun
                      Projectile
                        { projectileType = Gun,
                          projectilePos = (0, 0),
                          projectileSize = projectileSizeVar,
                          projectileVelocity = (10, 0),
                          projectileHealth = 1,
                          projectileDamage = 30,
                          projectileOrigin = Players,
                          projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate 90 $ getTexture "bullet" _assetList
                        },
                  airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player_1" _assetList
                }
            ],
          status = InGame,
          menu = initPauseMenu,
          currentLevel = _levels !! getLevelIndex _menu
        }

start2player :: GameState -> GameState
start2player gs@GameState {tmpassetList = _assetList, levels = _levels} =
  gs

-- { players =
--     [ Airplane
--         { airplaneType = Player1,
--           airplanePos = (-400, 0),
--           airplaneDestinationPos = (0, 0),
--           airplaneSize = airplaneSizeVar,
--           airplaneVelocity = (0, 0),
--           airplaneMaxVelocity = (-12, 12),
--           airplaneHealth = 100,
--           fireRate = Single 30.0,
--           timeLastShot = 0.0,
--           airplanePowerUps = [],
--           airplaneGun =
--             AirplaneGun
--               Projectile
--                 { projectileType = Gun,
--                   projectilePos = (0, 0),
--                   projectileSize = projectileSizeVar,
--                   projectileVelocity = (10, 0),
--                   projectileHealth = 1,
--                   projectileDamage = 30,
--                   projectileOrigin = Players,
--                   projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate 90 $ getTexture "bullet" _assetList
--                 },
--           airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player_1" _assetList
--         },
--       Airplane
--         { airplaneType = Player2,
--           airplanePos = (-200, 0),
--           airplaneDestinationPos = (0, 0),
--           airplaneSize = airplaneSizeVar,
--           airplaneVelocity = (0, 0),
--           airplaneMaxVelocity = (-12, 12),
--           airplaneHealth = 100,
--           fireRate = Single 30.0,
--           timeLastShot = 0.0,
--           airplanePowerUps = [],
--           airplaneGun =
--             AirplaneGun
--               Projectile
--                 { projectileType = Gun,
--                   projectilePos = (0, 0),
--                   projectileSize = projectileSizeVar,
--                   projectileVelocity = (10, 0),
--                   projectileHealth = 1,
--                   projectileDamage = 30,
--                   projectileOrigin = Players,
--                   projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate 90 $ getTexture "bullet" _assetList
--                 },
--           airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player_2" _assetList
--         }
--     ],
--   status = InGame,
--   projectiles = [],
--   currentLevel = head _levels,
--   enemies =
--     [ -- tmp enemy
--       Airplane
--         { airplaneType = Fighter,
--           airplanePos = (-10, -180),
--           airplaneDestinationPos = (0, 0),
--           airplaneSize = airplaneSizeVar,
--           airplaneVelocity = (0, 0),
--           airplaneMaxVelocity = (-12, 12),
--           airplaneHealth = 100,
--           fireRate = Burst 120.0,
--           timeLastShot = 0.0,
--           airplanePowerUps = [],
--           airplaneGun =
--             AirplaneGun
--               Projectile
--                 { projectileType = Gun,
--                   projectilePos = (0, 0),
--                   projectileSize = projectileSizeVar,
--                   projectileVelocity = (-10, 0),
--                   projectileHealth = 1,
--                   projectileDamage = 10,
--                   projectileOrigin = Enemies,
--                   projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate (-90) $ getTexture "double-bullet" _assetList
--                 },
--           airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate (-90) $ getTexture "fighter" _assetList
--         }
--     ],
--   powerUps = [],
--   particles = [],
--   menu = initPauseMenu
-- }