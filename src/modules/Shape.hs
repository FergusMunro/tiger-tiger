module Shape
  ( Direction (Direction),
    Shape (getCoordinates, getCentre),
    directionToAngle,
    areIntersecting,
    rotate2d,
  )
where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle (degToRad)

data Direction = Direction Int Int

class Shape a where
  getCoordinates :: a -> [Point]
  getCentre :: a -> Point

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

type Vector2d = (Float, Float) -- even though we could use point this makes the following function a bit clearer

areIntersecting :: ((Shape a), (Shape b)) => a -> b -> Bool
areIntersecting x y = all (overLap x y) axes
  where
    overLap a b v = aMax >= bMin && bMax >= aMin
      where
        (aMin, aMax) = project a v
        (bMin, bMax) = project b v

    axes = getAxes x ++ getAxes y

    getAxes :: (Shape a) => a -> [Vector2d]
    getAxes z = map getOrthogonalVec . snd . foldr f (c, []) $ (c : cs)
      where
        (c : cs) = getCoordinates z
        -- this folding function gets the line between two vectors, and stores in an accumulator
        f :: Point -> (Point, [Vector2d]) -> (Point, [Vector2d])
        f (a, b) ((c, d), vs) = ((a, b), (a - c, b - d) : vs)

    -- the two output floats represent the minimum and maximum values
    project :: (Shape a) => a -> Vector2d -> (Float, Float)
    project z v = (minimum projections, maximum projections)
      where
        projections = map (v `dot`) (getCoordinates z)

rotate2d :: Float -> Vector2d -> Vector2d
rotate2d degrees (x, y) =
  ( x * cos t - y * sin t,
    x * sin t + y * cos t
  )
  where
    t = degToRad degrees

getOrthogonalVec :: Vector2d -> Vector2d
getOrthogonalVec (x, y) = (-y, x)

dot :: Vector2d -> Vector2d -> Float
(a, b) `dot` (c, d) = a * c + b * d
