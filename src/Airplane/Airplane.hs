module Airplane where

import Config
import Model

-- TODO values in Config.hs
-- TODO Naming refactor
-- TODO handle if Airplane has prijectileType None <- Now a warning is given

shoot :: Airplane -> [Projectile]
shoot Airplane {airplanePos = (x, y), fireRate = r, airplaneGun = (AirplaneGun projectile)} = case r of
  Single _ -> [projectile {projectilePos = (x + gunOffset, y - gunOffset)}]
  Burst _ -> [projectile {projectilePos = (x - px - 5 + gunOffset, y - gunOffset)}, projectile {projectilePos = (x + gunOffset, y - gunOffset)}, projectile {projectilePos = (x - px - px - 10 + gunOffset, y - gunOffset)}] -- TODO: update: - or + is actually depended on if its a player or enemy
    where
      (px, _) = projectileSize projectile

minMax :: (Float, Float) -> Float -> Float
minMax (minValue, maxValue) value = min maxValue (max minValue value)