module Init where

import Assets
import Config
import Data.Map as Dict
import qualified Data.Set as S
import Graphics.Gloss
import Model

debugInitLevel :: Assets -> Level
debugInitLevel assetlist = Level {levelNr = 1, waves = [Wave [createBasicEnemy Fighter (500, -350) assetlist, createBasicEnemy Fighter (300, -200) assetlist] 200, Wave [createBasicEnemy Fighter (500, 350) assetlist] 200, Wave [createBasicEnemy Fighter (300, 100) assetlist] 200]}

initialState :: Assets -> GameState
initialState assetlist =
  GameState
    { elapsedTime = 0,
      status = InMenu,
      players = [],
      enemies = [],
      levels = Level 0 [],
      projectiles = [],
      powerUps = [],
      pressedKeys = S.empty,
      menu = initMenu,
      particles = [],
      particleMap =
        Dict.fromList
          [ ( "explosion",
              Particle
                { particlePosition = (0, 0),
                  particleSize = (0, 0),
                  particleInterval = 8,
                  particleTimer = 8,
                  particleSprites = [getTexture "explotion_1" assetlist, getTexture "explotion_2" assetlist, getTexture "explotion_3" assetlist, getTexture "explotion_4" assetlist, getTexture "explotion_5" assetlist]
                }
            ),
            ( "explosion2",
              Particle
                { particlePosition = (0, 0),
                  particleSize = (0, 0),
                  particleInterval = 8,
                  particleTimer = 8,
                  particleSprites = [Scale 2.0 2.0 $ getTexture "explotion_1" assetlist, Scale 2.0 2.0 $ getTexture "explotion_2" assetlist, Scale 2.0 2.0 $ getTexture "explotion_3" assetlist, Scale 2.0 2.0 $ getTexture "explotion_4" assetlist, Scale 2.0 2.0 $ getTexture "explotion_5" assetlist]
                }
            ),
            ( "5SecondTimer",
              Particle
                { particlePosition = (-400, 80),
                  particleSize = (10, 10),
                  particleInterval = 60,
                  particleTimer = 60,
                  particleSprites = [getTexture "5" assetlist, getTexture "4" assetlist, getTexture "3" assetlist, getTexture "2" assetlist, getTexture "1" assetlist]
                }
            )
          ],
      tmpassetList = assetlist
    }

initMenu :: Menu
initMenu =
  Menu
    { fields = [playField, creditsField, exitField],
      returnMenu = NoMenu
    }
  where
    playField, creditsField, exitField :: Field
    playField = Field {fieldName = "Play", fieldPosition = (0, 200), subMenu = initPlayMenu}

    creditsField = Field {fieldName = "Credits", fieldPosition = (0, 0), subMenu = NoMenu}

    exitField = Field {fieldName = "Exit", fieldPosition = (0, -200), subMenu = NoMenu}

initPlayMenu :: Menu
initPlayMenu =
  Menu
    { fields = [onePlayer, twoPlayer],
      returnMenu = initMenu
    }
  where
    onePlayer, twoPlayer :: Field
    onePlayer = Field {fieldName = "1 Player", fieldPosition = (0, 200), subMenu = NoMenuButFunction start1player}

    twoPlayer = Field {fieldName = "2 Player", fieldPosition = (0, 0), subMenu = NoMenuButFunction start2player}

initPauseMenu :: Menu
initPauseMenu =
  Menu
    { fields = [resume, quit],
      returnMenu = NoMenu
    }
  where
    resume, quit :: Field
    resume = Field {fieldName = "Resume", fieldPosition = (0, 200), subMenu = NoMenuButFunction resumeGame}

    quit = Field {fieldName = "Quit", fieldPosition = (0, 0), subMenu = initMenu}

initVictoryMenu :: Menu
initVictoryMenu =
  Menu
    { fields = [nextLevel, selectLevel, mainMenu],
      returnMenu = NoMenu
    }
  where
    nextLevel, selectLevel, mainMenu :: Field
    nextLevel = Field {fieldName = "Next Level", fieldPosition = (0, 200), subMenu = NoMenuButFunction start1player}

    selectLevel = Field {fieldName = "Select Level", fieldPosition = (0, 0), subMenu = initPlayMenu}

    mainMenu = Field {fieldName = "Main Menu", fieldPosition = (0, -200), subMenu = initMenu}

initDefeatMenu :: Menu
initDefeatMenu =
  Menu
    { fields = [retryLevel, selectLevel, mainMenu],
      returnMenu = NoMenu
    }
  where
    retryLevel, selectLevel, mainMenu :: Field
    retryLevel = Field {fieldName = "Retry Level", fieldPosition = (0, 200), subMenu = NoMenuButFunction start1player}

    selectLevel = Field {fieldName = "Select Level", fieldPosition = (0, 0), subMenu = initPlayMenu}

    mainMenu = Field {fieldName = "Main Menu", fieldPosition = (0, -200), subMenu = initMenu}

resumeGame :: GameState -> GameState
resumeGame gs = gs {status = InGame}

-- Toggles the status in the GameState.
start1player :: GameState -> GameState
start1player gs@GameState {tmpassetList = _assetList} =
  gs
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
      projectiles = [],
      enemies =
        [ Airplane
            { airplaneType = Kamikaze,
              airplanePos = (400, 200),
              airplaneDestinationPos = (500, 200),
              airplaneSize = airplaneSizeVar,
              airplaneVelocity = (-4, 0),
              airplaneMaxVelocity = (-5, 5),
              airplaneHealth = 100,
              fireRate = Burst 1200000.0,
              timeLastShot = 0.0,
              airplanePowerUps = [],
              airplaneGun = None,
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ getTexture "ship_0000" _assetList
            }
        ],
      levels = debugInitLevel _assetList,
      powerUps =
        [ PowerUp
            { powerUpPos = (-400, 70),
              powerUpSize = (10, 10),
              powerUpType = PowerPack 0.0125,
              powerUpState = WorldSpace,
              timeUntilDespawn = 1000.0,
              powerUpDuration = 500.0,
              powerUpSprites =
                Sprites
                  { spritesState = Idle,
                    spritePos = (0, 0),
                    spritesInterval = 10.0,
                    spritesTimer = 10.0,
                    idleSprites = [getTexture "power-pack_1" _assetList, getTexture "power-pack_2" _assetList],
                    movingSprites = []
                  }
            }
        ],
      particles = [],
      menu = initPauseMenu
    }

start2player :: GameState -> GameState
start2player gs@GameState {tmpassetList = _assetList} =
  gs
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
            },
          Airplane
            { airplaneType = Player2,
              airplanePos = (-200, 0),
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
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player_2" _assetList
            }
        ],
      status = InGame,
      projectiles = [],
      levels = debugInitLevel _assetList,
      enemies =
        [ -- tmp enemy
          Airplane
            { airplaneType = Fighter,
              airplanePos = (-10, -180),
              airplaneDestinationPos = (0, 0),
              airplaneSize = airplaneSizeVar,
              airplaneVelocity = (0, 0),
              airplaneMaxVelocity = (-12, 12),
              airplaneHealth = 100,
              fireRate = Burst 120.0,
              timeLastShot = 0.0,
              airplanePowerUps = [],
              airplaneGun =
                AirplaneGun
                  Projectile
                    { projectileType = Gun,
                      projectilePos = (0, 0),
                      projectileSize = projectileSizeVar,
                      projectileVelocity = (-10, 0),
                      projectileHealth = 1,
                      projectileDamage = 10,
                      projectileOrigin = Enemies,
                      projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate (-90) $ getTexture "double-bullet" _assetList
                    },
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate (-90) $ getTexture "ship_0000" _assetList
            }
        ],
      powerUps = [],
      particles = [],
      menu = initPauseMenu
    }

createBasicEnemy :: AirPlaneType -> Position -> Assets -> Enemy
createBasicEnemy enemytype position assetList =
  Airplane
    { airplaneType = enemytype,
      airplanePos = position,
      airplaneDestinationPos = (0, 0),
      airplaneSize = airplaneSizeVar,
      airplaneVelocity = (0, 0),
      airplaneMaxVelocity = (-12, 12),
      airplaneHealth = 100,
      fireRate = Burst 120.0,
      timeLastShot = 0.0,
      airplanePowerUps = [],
      airplaneGun =
        AirplaneGun
          Projectile
            { projectileType = Gun,
              projectilePos = (0, 0),
              projectileSize = projectileSizeVar,
              projectileVelocity = (-10, 0),
              projectileHealth = 1,
              projectileDamage = 10,
              projectileOrigin = Enemies,
              projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate (-90) $ getTexture "double-bullet" assetList
            },
      airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate (-90) $ getTexture "ship_0000" assetList
    }
