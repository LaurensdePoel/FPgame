{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Map as Map
import Data.Maybe
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Key)

data Status = InMenu | InGame deriving (Eq)

type Position = Point

-- This is unnecessary
instance Num Point where
  (+) :: Point -> Point -> Point
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  (-) :: Point -> Point -> Point
  (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  (*) :: Point -> Point -> Point
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  signum :: Point -> Point
  signum (x, y) = (signum x, signum y)
  abs :: Point -> Point
  abs (x, y) = (abs x, abs y)
  negate :: Point -> Point
  negate (x, y) = (negate x, negate y)
  fromInteger :: Integer -> Point
  fromInteger x = (fromInteger x, fromInteger x)

fps :: Int
fps = 60

newtype Size = Size Point

type Time = Float

type Velocity = Point

data ProjectileType = Gun | DoubleGun | Rocket

type Damage = Int

data Origin = Players | Enemies deriving (Eq)

-- firerate timelastshot
data FireRate = Single Time | Burst Time

newtype ScreenBox = ScreenBox (Point, Point)

data AirPlaneType = Player1 | Player2 | Fighter | Kamikaze | FlyBy deriving (Eq)

data PowerUpTypes = HealthPack Int | PowerPack Float

data PowerUpState = WorldSpace | PickedUp

data AnimationState = Idle | Moving

data AirplaneGun = AirplaneGun Projectile | None

data Sprites = Sprites
  { spritesState :: AnimationState,
    spritePos :: Position,
    spritesInterval :: Time,
    spritesTimer :: Time,
    idleSprites :: [Picture],
    movingSprites :: [Picture]
  }

data Particle = Particle
  { particlePosition :: Position,
    particleSize :: Size,
    particleInterval :: Time,
    particleTimer :: Time,
    particleSprites :: [Picture]
  }

data PowerUp = PowerUp
  { powerUpPos :: Position,
    powerUpSize :: Size,
    powerUpType :: PowerUpTypes,
    powerUpState :: PowerUpState,
    timeUntilDespawn :: Time,
    powerUpDuration :: Time,
    -- powerUpSprite :: Picture,
    powerUpSprites :: Sprites
  }

data Airplane = Airplane
  { airplaneType :: AirPlaneType,
    airplanePos :: Position,
    airplaneDestinationPos :: Position,
    airplaneSize :: Size,
    airplaneVelocity :: Velocity,
    airplaneMaxVelocity :: Velocity,
    airplaneHealth :: Int,
    fireRate :: FireRate,
    timeLastShot :: Time,
    airplaneGun :: AirplaneGun,
    airplaneSprite :: Picture,
    airplanePowerUps :: [PowerUp]
  }

data Projectile = Projectile
  { projectileType :: ProjectileType,
    projectilePos :: Position,
    projectileSize :: Size,
    projectileVelocity :: Velocity,
    projectileHealth :: Int,
    projectileDamage :: Damage,
    projectileOrigin :: Origin,
    projectileSprite :: Picture
  }

data GameState = Game
  { elapsedTime :: Float,
    status :: Status,
    players :: [Airplane],
    enemies :: [Airplane],
    -- level :: Level,
    projectiles :: [Projectile],
    powerUps :: [PowerUp],
    particleMap :: Map String Particle,
    particles :: [Particle],
    pressedKeys :: S.Set Key,
    menu :: Menu,
    tmpassetList :: Map String Picture
  }

-- TODO add to glabal file
projectileSizeXY, airplaneSizeXY, gunOffset :: Float
projectileSizeXY = 16.0
airplaneSizeXY = 32.0
gunOffset = airplaneSizeXY * 0.5 - projectileSizeXY * 0.5

projectileSizeVar, airplaneSizeVar :: Size
projectileSizeVar = Size (projectileSizeXY, projectileSizeXY)
airplaneSizeVar = Size (airplaneSizeXY, airplaneSizeXY)

initialState :: Map String Picture -> GameState
initialState assetlist =
  Game
    { elapsedTime = 0,
      status = InMenu,
      players = [],
      enemies = [],
      projectiles = [],
      powerUps = [],
      pressedKeys = S.empty,
      menu = initMenu,
      particles = [],
      particleMap =
        Map.fromList
          [ ( "explosion",
              Particle
                { particlePosition = (0, 0),
                  particleSize = Size (0, 0),
                  particleInterval = 8,
                  particleTimer = 8,
                  particleSprites = [getTexture "explosionPart1" assetlist, getTexture "explosionPart2" assetlist, getTexture "explosionPart3" assetlist, getTexture "explosionPart4" assetlist, getTexture "explosionPart5" assetlist]
                }
            ),
            ( "explosion2",
              Particle
                { particlePosition = (0, 0),
                  particleSize = Size (0, 0),
                  particleInterval = 8,
                  particleTimer = 8,
                  particleSprites = [getTexture "explosion2Part1" assetlist, getTexture "explosion2Part2" assetlist, getTexture "explosion2Part3" assetlist, getTexture "explosion2Part4" assetlist, getTexture "explosion2Part5" assetlist]
                }
            ),
            ( "5SecondTimer",
              Particle
                { particlePosition = (-400, 80),
                  particleSize = Size (10, 10),
                  particleInterval = 60,
                  particleTimer = 60,
                  particleSprites = [getTexture "5" assetlist, getTexture "4" assetlist, getTexture "3" assetlist, getTexture "2" assetlist, getTexture "1" assetlist]
                }
            )
          ],
      tmpassetList =
        assetlist
        {-players =
                [ Airplane
                    { airplaneType = Player1,
                      airplanePos = (-400, 0),
                      airplaneSize = airplaneSizeVar,
                      airplaneVelocity = (0, 0),
                      airplaneHealth = 100,
                      fireRate = Single 30.0,
                      timeLastShot = 0.0,
                      airplanePowerUps = [],
                      airplaneProjectile =
                        Projectile
                          { projectileType = Gun,
                            projectilePos = (0, 0),
                            projectileSize = projectileSizeVar,
                            projectileVelocity = (10, 0),
                            projectileHealth = 1,
                            projectileDamage = 30,
                            projectileOrigin = Players,
                            projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate 90 $ getTexture "bullet" assetlist
                          },
                      airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player1" assetlist
                    },
                  Airplane
                    { airplaneType = Player2,
                      airplanePos = (-200, 0),
                      airplaneSize = airplaneSizeVar,
                      airplaneVelocity = (0, 0),
                      airplaneHealth = 100,
                      fireRate = Single 30.0,
                      timeLastShot = 0.0,
                      airplanePowerUps = [],
                      airplaneProjectile =
                        Projectile
                          { projectileType = Gun,
                            projectilePos = (0, 0),
                            projectileSize = projectileSizeVar,
                            projectileVelocity = (10, 0),
                            projectileHealth = 1,
                            projectileDamage = 30,
                            projectileOrigin = Players,
                            projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate 90 $ getTexture "bullet" assetlist
                          },
                      airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player2" assetlist
                    }
                ],
              enemies =
                [ -- tmp enemy
                  Airplane
                    { airplaneType = Fighter,
                      airplanePos = (-10, -180),
                      airplaneSize = airplaneSizeVar,
                      airplaneVelocity = (0, 0),
                      airplaneHealth = 100,
                      fireRate = Burst 120.0,
                      timeLastShot = 0.0,
                      airplanePowerUps = [],
                      airplaneProjectile =
                        Projectile
                          { projectileType = Gun,
                            projectilePos = (0, 0),
                            projectileSize = projectileSizeVar,
                            projectileVelocity = (-10, 0),
                            projectileHealth = 1,
                            projectileDamage = 10,
                            projectileOrigin = Enemies,
                            projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate (-90) $ getTexture "bullet" assetlist
                          },
                      airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate (-90) $ getTexture "player1" assetlist
                    }
                ],
              tmpInt = 0,
              pressedKeys = S.empty,
              projectiles = [],
              powerUps =
                [ PowerUp
                    { powerUpPos = (-400, 70),
                      powerUpSize = Size (10, 10),
                      powerUpType = PowerPack 0.0125,
                      powerUpState = WorldSpace,
                      timeUntilDespawn = 300.0,
                      powerUpDuration = 300.0,
                      -- powerUpSprite = flip fixImageOrigin airplaneSizeVar $ getTexture "powerPack" assetlist,
                      powerUpSprites =
                        Sprites
                          { spritesState = Idle,
                            spritePos = (0, 0),
                            spritesInterval = 10.0,
                            spritesTimer = 10.0,
                            idleSprites = [getTexture "powerPackSprite1" assetlist, getTexture "powerPackSprite2" assetlist],
                            movingSprites = []
                          }
                    }
                ],
        <<<<<<< HEAD
              particles = [],
              particleMap =
                Map.fromList
                  [ ( "explosion",
                      Particle
                        { particlePosition = (0, 0),
                          particleSize = Size (0, 0),
                          particleInterval = 8,
                          particleTimer = 8,
                          particleSprites = [getTexture "explosionPart1" assetlist, getTexture "explosionPart2" assetlist, getTexture "explosionPart3" assetlist, getTexture "explosionPart4" assetlist, getTexture "explosionPart5" assetlist]
                        }
                    ),
                    ( "explosion2",
                      Particle
                        { particlePosition = (0, 0),
                          particleSize = Size (0, 0),
                          particleInterval = 8,
                          particleTimer = 8,
                          particleSprites = [getTexture "explosion2Part1" assetlist, getTexture "explosion2Part2" assetlist, getTexture "explosion2Part3" assetlist, getTexture "explosion2Part4" assetlist, getTexture "explosion2Part5" assetlist]
                        }
                    ),
                    ( "5SecondTimer",
                      Particle
                        { particlePosition = (-400, 80),
                          particleSize = Size (10, 10),
                          particleInterval = 60,
                          particleTimer = 60,
                          particleSprites = [getTexture "5" assetlist, getTexture "4" assetlist, getTexture "3" assetlist, getTexture "2" assetlist, getTexture "1" assetlist]
                        }
                    )
                  ]
        =======
                -}
        -- >>>>>>> main
    }

getTexture :: String -> Map String Picture -> Picture
getTexture s m = case Map.lookup s m of
  Nothing -> rotate (-90) $ Scale 0.25 0.25 (color red $ Text "error")
  Just x -> x

getParticle :: String -> Map String Particle -> Particle
getParticle key _map = fromMaybe Particle {particlePosition = (10, 10), particleSize = Size (10, 10), particleInterval = 60, particleTimer = 60, particleSprites = [Scale 0.25 0.25 (color red $ Text "error")]} (Map.lookup key _map)

-- TODO move to view and fix apply to all images when loading for the first time
fixImageOrigin :: Picture -> Size -> Picture
fixImageOrigin pic (Size (x, y)) = translate (x * 0.5) (y * (-0.5)) pic

data Menu
  = Menu
      { -- header :: Picture,
        fields :: [Field],
        -- menuBackground :: Picture,
        returnMenu :: Menu
      }
  | NoMenu
  | NoMenuButFunction (GameState -> GameState)

data Field = Field
  { fieldName :: String,
    fieldPosition :: Position,
    subMenu :: Menu
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

resumeGame :: GameState -> GameState
resumeGame gs = gs {status = InGame}

-- Toggles the status in the GameState.
start1player :: GameState -> GameState
start1player gs@Game {tmpassetList = _assetList} =
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
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player1" _assetList
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
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ getTexture "player1" _assetList
            }
        ],
      powerUps =
        [ PowerUp
            { powerUpPos = (-400, 70),
              powerUpSize = Size (10, 10),
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
                    idleSprites = [getTexture "powerPackSprite1" _assetList, getTexture "powerPackSprite2" _assetList],
                    movingSprites = []
                  }
            }
        ],
      particles = [],
      menu = initPauseMenu
    }

start2player :: GameState -> GameState
start2player gs@Game {tmpassetList = _assetList} =
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
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player1" _assetList
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
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player2" _assetList
            }
        ],
      status = InGame,
      projectiles = [],
      enemies =
        [ Airplane
            { airplaneType = FlyBy,
              airplanePos = (400, 200),
              airplaneDestinationPos = (0, 0),
              airplaneSize = airplaneSizeVar,
              airplaneVelocity = (-4, 0),
              airplaneMaxVelocity = (-6, 6),
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
                      projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate (-90) $ getTexture "bullet" _assetList
                    },
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate (-90) $ getTexture "player1" _assetList
            },
          Airplane
            { airplaneType = Fighter,
              airplanePos = (400, -200),
              airplaneDestinationPos = (0, 0),
              airplaneSize = airplaneSizeVar,
              airplaneVelocity = (-2, 0),
              airplaneMaxVelocity = (-3, 3),
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
                      projectileVelocity = (-10, 0),
                      projectileHealth = 1,
                      projectileDamage = 10,
                      projectileOrigin = Enemies,
                      projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate (-90) $ getTexture "bullet" _assetList
                    },
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate (-90) $ getTexture "player1" _assetList
            },
          Airplane
            { airplaneType = Kamikaze,
              airplanePos = (500, -400),
              airplaneDestinationPos = (0, 0),
              airplaneSize = airplaneSizeVar,
              airplaneVelocity = (-2, 0),
              airplaneMaxVelocity = (-9, 9),
              airplaneHealth = 500,
              fireRate = Single 3000000.0,
              timeLastShot = 0.0,
              airplanePowerUps = [],
              airplaneGun = None,
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate (-90) $ getTexture "player2" _assetList
            }
        ],
      powerUps = [],
      particles = [],
      menu = initPauseMenu
    }
