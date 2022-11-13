-- | Defines helper functions which can be used through out the project
module Helper where

-- | Bounds value between range
minMax :: (Float, Float) -> Float -> Float
minMax (minValue, maxValue) value = min maxValue (max minValue value)