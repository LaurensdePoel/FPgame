{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Map as Map
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

data ProjectileType = None | Gun | DoubleGun | Rocket

type Damage = Int

data Origin = Players | Enemies deriving (Eq)

-- firerate timelastshot
data FireRate = Single Time | Burst Time

newtype ScreenBox = ScreenBox (Point, Point)

data AirPlaneType = Player1 | Player2 | Fighter | Kamikaze deriving (Eq)

data PowerUpTypes = HealthPack Int | PowerPack Float

data PowerUpState = WorldSpace | PickedUp

data PowerUp = PowerUp
  { powerUpPos :: Position,
    powerUpSize :: Size,
    powerUpType :: PowerUpTypes,
    powerUpState :: PowerUpState,
    timeUntilDespawn :: Time,
    powerUpDuration :: Time,
    powerUpSprite :: Picture
  }

data Airplane = Airplane
  { airplaneType :: AirPlaneType,
    airplanePos :: Position,
    airplaneSize :: Size,
    airplaneVelocity :: Velocity,
    airplaneHealth :: Int,
    fireRate :: FireRate,
    timeLastShot :: Time,
    airplaneProjectile :: Projectile,
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
      tmpassetList = assetlist
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
              timeUntilDespawn = 1000.0,
              powerUpDuration = 500.0,
              powerUpSprite = flip fixImageOrigin airplaneSizeVar $ getTexture "powerPack" assetlist
            }
        ],
        -}
    }

getTexture :: String -> Map String Picture -> Picture
getTexture s m = case Map.lookup s m of
  Nothing -> rotate (-90) $ Scale 0.25 0.25 (color red $ Text "error")
  Just x -> x

-- TODO move to view and fix apply to all images when loading for the first time
fixImageOrigin :: Picture -> Size -> Picture
fixImageOrigin pic (Size (x, y)) = translate (x * 0.5) (y * (-0.5)) pic

data Menu
  = Menu
      { --header :: Picture,
        fields :: [Field],
        --menuBackground :: Picture,
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
                    projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate 90 $ getTexture "bullet" _assetList
                  },
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player1" _assetList
            }
        ],
      status = InGame,
      projectiles = [],
      enemies = [],
      menu = initPauseMenu
    }

start2player :: GameState -> GameState
start2player gs@Game {tmpassetList = _assetList} =
  gs
    { players =
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
                    projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate 90 $ getTexture "bullet" _assetList
                  },
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player1" _assetList
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
                    projectileSprite = flip fixImageOrigin projectileSizeVar $ rotate 90 $ getTexture "bullet" _assetList
                  },
              airplaneSprite = flip fixImageOrigin airplaneSizeVar $ rotate 90 $ getTexture "player2" _assetList
            }
        ],
      status = InGame,
      projectiles = [],
      enemies = [],
      menu = initPauseMenu
    }
