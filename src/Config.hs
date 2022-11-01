module Config where

import Model

-- TODO Add all values

fps :: Int
fps = 60

offset :: Int
offset = 100

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

projectileSizeXY, airplaneSizeXY, gunOffset :: Float
projectileSizeXY = 16.0
airplaneSizeXY = 32.0
gunOffset = airplaneSizeXY * 0.5 - projectileSizeXY * 0.5

projectileSizeVar, airplaneSizeVar :: Size
projectileSizeVar = (projectileSizeXY, projectileSizeXY)
airplaneSizeVar = (airplaneSizeXY, airplaneSizeXY)