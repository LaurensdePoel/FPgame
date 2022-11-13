module Airplane where

import Model
import Updateable (Updateable (getCenterPosition))

-- | Returns a list of new projectiles based on the airplaneGun
shoot :: Airplane -> [Projectile]
shoot Airplane {airplaneGun = None} = []
shoot airplane@Airplane {airplaneType = _type, fireRate = _fireRate, airplaneGun = (AirplaneGun projectile)} =
  case _fireRate of
    Single _ -> [projectile {projectilePos = gunOffset}]
    Burst _ ->
      [ projectile {projectilePos = gunOffset},
        projectile {projectilePos = applyXOffset _type gunOffset (projectileX + projectileY)},
        projectile {projectilePos = applyXOffset _type gunOffset (projectileX * 2 + projectileY * 2)}
      ]
  where
    gunOffset :: Position
    gunOffset = getCenterPosition airplane - getCenterPosition projectile
    (projectileX, projectileY) = projectileSize projectile

    applyXOffset :: AirPlaneType -> Position -> Float -> Position
    applyXOffset type' (x, y) offset'
      | type' == Player1 || type' == Player2 = (x + offset', y)
      | otherwise = (x - offset', y)