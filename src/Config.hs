module Config where

import Model

-- TODO Add all values

-- * Window

fps :: Int
fps = 60

offset :: Int
offset = 0

-- ** Screen size

screenWidth :: Int
screenWidth = 1024

screenHeight :: Int
screenHeight = 768

screenMaxX :: Float
screenMaxX = fromIntegral screenWidth * 0.5

screenMinX :: Float
screenMinX = fromIntegral screenWidth * (-0.5)

screenMaxY :: Float
screenMaxY = fromIntegral screenHeight * 0.5

screenMinY :: Float
screenMinY = fromIntegral screenHeight * (-0.5)

-- * Menu

menuTextOffset :: Float
menuTextOffset = -200

menuTextStartHeight :: Float
menuTextStartHeight = 200

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

behaviourVelocitySteps :: Float
behaviourVelocitySteps = 0.25

-- * Multipliers

velocityReduction :: Float
velocityReduction = 0.2

destinationErrorMargin :: (Float,Float)
destinationErrorMargin = (0.2,0.2)