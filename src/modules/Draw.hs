module Draw where

import Graphics.Gloss

data Direction = Direction Int Int

directionToAngle :: Direction -> Float
directionToAngle (Direction 1 0) = 0 -- right
directionToAngle (Direction 1 (-1)) = 45 -- up-right
directionToAngle (Direction 0 (-1)) = 90 -- up
directionToAngle (Direction (-1) (-1)) = 135 -- up-left
directionToAngle (Direction (-1) 0) = 180 -- left
directionToAngle (Direction (-1) 1) = 225 -- down-left
directionToAngle (Direction 0 1) = 270 -- down
directionToAngle (Direction 1 1) = 315 -- down-right
directionToAngle (Direction 0 0) = 0 -- default to right

data SpriteSheet = SpriteSheet
  { playerSprite :: Picture,
    sharkSprite :: Picture,
    jellyFishSprite :: Picture
  }

class Draw a where
  draw :: SpriteSheet -> a -> Picture
