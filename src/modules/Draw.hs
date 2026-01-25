module Draw where

import Graphics.Gloss

data Direction = Direction Int Int

newtype SpriteSheet = SpriteSheet
  { playerSprite :: Picture
  }

class Draw a where
  draw :: SpriteSheet -> a -> Picture
