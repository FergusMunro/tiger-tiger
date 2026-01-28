module Draw where

import Graphics.Gloss

data SpriteSheet = SpriteSheet
  { playerSprite :: Picture,
    sharkSprite :: Picture,
    jellyFishSprite :: Picture,
    backgroundSprite :: Picture,
    oxygenSprite :: Picture,
    treasureSprite :: Picture,
    powerUpSprite :: Picture,
    blockSprite :: Picture
  }

class Draw a where
  draw :: SpriteSheet -> a -> Picture

hudColor :: Color
hudColor = makeColorI 10 35 38 255

-- NOTE: Important Constants
maxWidth :: Float
maxWidth = 1920

screenWidth :: Float
screenWidth = 1000

screenHeight :: Float
screenHeight = 1080
