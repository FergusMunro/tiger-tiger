module Shape
  ( Direction (Direction),
    Shape (getCoordinates, getCentre, translateShape),
    directionToAngle,
    areIntersecting,
    findMTV,
    rotate2d,
  )
where

import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PointArithmetic
import Graphics.Gloss.Geometry.Angle (degToRad)

data Direction = Direction Int Int

class Shape a where
  getCoordinates :: a -> [Point]
  getCentre :: a -> Point
  translateShape :: Point -> a -> a

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

type Vector2d = (Float, Float) -- even though we could use point this makes the following functions a bit clearer

areIntersecting :: ((Shape a), (Shape b)) => a -> b -> Bool
areIntersecting x y = all (areOverlapping x y) axes
  where
    areOverlapping a b v = aMax >= bMin && bMax >= aMin
      where
        (aMin, aMax) = project a v
        (bMin, bMax) = project b v

    axes = getAxes x ++ getAxes y

findMTV :: ((Shape a), (Shape b)) => a -> b -> Point
findMTV x y = direction * overlap PointArithmetic.* axis
  where
    direction = signum ((getCentre y PointArithmetic.- getCentre x) `dot` axis)
    (overlap, axis) = minimumBy (comparing fst) axesWithOverlap
    axesWithOverlap = zip (map (getOverlap x y) axes) axes
    -- because we only call this when areIntersecting  is true, we know the overlap will be non-zero
    getOverlap a b v = min aMax bMax - max aMin bMin
      where
        (aMin, aMax) = project a v
        (bMin, bMax) = project b v

    axes = map normalize $ getAxes x ++ getAxes y

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

normalize :: Vector2d -> Vector2d
normalize (x, y) = (x / m, y / m)
  where
    m = sqrt (x ^ 2 + y ^ 2)

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
