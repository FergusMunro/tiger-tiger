module Draw where

import Graphics.Gloss

data SpriteSheet = SpriteSheet
  { playerSprite :: Picture,
    ascendingPlayerSprite :: Picture,
    sharkSprites :: [Picture],
    jellyFishSprites :: [Picture],
    redSharkSprites :: [Picture],
    turtleSprites :: [Picture],
    backgroundSprite :: Picture,
    oxygenSprite :: Picture,
    treasureSprite :: Picture,
    bigTreasureSprite :: Picture,
    powerUpSprite :: Picture,
    crystalSprite :: Picture,
    blockSprite :: Picture
  }

class Draw a where
  draw :: SpriteSheet -> a -> Picture

hudColor :: Color
hudColor = makeColorI 10 35 38 255

-- NOTE: Important Constants
maxWidth :: Float
maxWidth = 1920

maxHeight :: Float
maxHeight = 1080

screenWidth :: Float
screenWidth = 1000

screenHeight :: Float
screenHeight = 800

backgroundSpriteWidth :: Float
backgroundSpriteWidth = 1024

backgroundSpriteHeight :: Float
backgroundSpriteHeight = 1280

jellyAnimationLength :: Int
jellyAnimationLength = 17

sharkAnimationLength :: Int
sharkAnimationLength = 30

turtleAnimationLength :: Int
turtleAnimationLength = 24
