module Collision where

import Graphics.Gloss.Geometry.Angle (degToRad)

data Direction = Direction Int Int

directionToAngle :: Direction -> Float
directionToAngle (Direction 1 0) = 0
directionToAngle (Direction 1 1) = 45
directionToAngle (Direction 0 1) = 90
directionToAngle (Direction (-1) 1) = 135
directionToAngle (Direction (-1) 0) = 180
directionToAngle (Direction (-1) (-1)) = 225
directionToAngle (Direction 0 (-1)) = 270
directionToAngle (Direction 1 (-1)) = 315
directionToAngle (Direction 0 0) = 0 -- default to right

type Coordinate = (Float, Float)

type Vector = (Float, Float)

rotate2d :: Float -> Vector -> Vector
rotate2d degrees (x, y) =
  ( x * cos t - y * sin t,
    x * sin t + y * cos t
  )
  where
    t = degToRad degrees

getOrthogonalVec :: Vector -> Vector
getOrthogonalVec (x, y) = (-y, x)

dot :: Vector -> Vector -> Float
(a, b) `dot` (c, d) = a * c + b * d

class Shape a where
  getCoordinates :: a -> [Coordinate]

-- all :: (a -> Bool) -> [a] -> [Bool]

areIntersecting :: ((Shape a), (Shape b)) => a -> b -> Bool
areIntersecting x y = all (overLap x y) axes
  where
    overLap a b v = aMax >= bMin && bMax >= aMin
      where
        (aMin, aMax) = project a v
        (bMin, bMax) = project b v

    axes = getAxes x ++ getAxes y

    getAxes :: (Shape a) => a -> [Vector]
    getAxes z = map getOrthogonalVec . snd . foldr f (c, []) $ (c : cs)
      where
        (c : cs) = getCoordinates z
        f :: Coordinate -> (Coordinate, [Vector]) -> (Coordinate, [Vector])
        f (a, b) ((c, d), vs) = ((a, b), (a - c, b - d) : vs)

    -- the two output floats represent the minimum and maximum values
    project :: (Shape a) => a -> Vector -> (Float, Float)
    project z v = (minimum projections, maximum projections)
      where
        projections = map (v `dot`) (getCoordinates z)
