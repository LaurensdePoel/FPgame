module Random where

import Model
import System.Random

getRandomPoint :: (Float, Float) -> (Float, Float) -> StdGen -> Position
getRandomPoint rangeX rangeY gen = (x, y)
  where
    (x, gen2) = randomR rangeX gen
    (y, _) = randomR rangeY gen2
