module Draw where

import Graphics.Gloss

data SpriteSheet = SpriteSheet
  { playerSprite :: Picture,
    sharkSprite :: Picture,
    jellyFishSprite :: Picture
  }

class Draw a where
  draw :: SpriteSheet -> a -> Picture
