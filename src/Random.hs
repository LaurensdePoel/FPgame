module Random where

import Model
import System.Random

-- | Generates a random position
getRandomPoint :: (Float, Float) -> (Float, Float) -> StdGen -> Position
getRandomPoint rangeX rangeY gen = (x, y)
  where
    (x, gen') = randomR rangeX gen
    (y, _) = randomR rangeY gen'