module Config where

import Model

-- TODO Add all values

-- * Window

fps :: Int
fps = 60

offset :: Int
offset = 100

-- ** Screen size

screenWidth :: Int
screenWidth = 1124

screenHeight :: Int
screenHeight = 868

screenMaxX :: Float
screenMaxX = fromIntegral screenWidth * 0.5

screenMinX :: Float
screenMinX = fromIntegral screenWidth * (-0.5)

screenMaxY :: Float
screenMaxY = fromIntegral screenHeight * 0.5

screenMinY :: Float
screenMinY = fromIntegral screenHeight * (-0.5)

-- * Sprite sizes and offsets

projectileSizeXY, airplaneSizeXY, gunOffset :: Float
projectileSizeXY = 16.0
airplaneSizeXY = 32.0
gunOffset = airplaneSizeXY * 0.5 - projectileSizeXY * 0.5

projectileSizeVar, airplaneSizeVar :: Size
projectileSizeVar = (projectileSizeXY, projectileSizeXY)
airplaneSizeVar = (airplaneSizeXY, airplaneSizeXY)

-- * Airplane base values

-- ** Player base values

velocityStep :: Float
velocityStep = 0.6

-- ** Enemey base values