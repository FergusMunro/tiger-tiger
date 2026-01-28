module Block
  ( Block,
    startingBlocks,
    createBlock,
    translateBlock,
  )
where

import Draw
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PointArithmetic
import Shape

data Block = Block
  { blockX :: Float,
    blockY :: Float
  }

instance Draw Block where
  draw ss b = Translate (blockX b) (blockY b) $ Scale 3.125 3.125 $ blockSprite ss

instance Shape Block where
  getCoordinates b =
    [ (x' - w, y' + h),
      (x' + w, y' + h),
      (x' + w, y' - h),
      (x' - w, y' - h)
    ]
    where
      x' = blockX b
      y' = blockY b
      w = blockWidth / 2
      h = blockHeight / 2

  getCentre b = (blockX b, blockY b)

startingBlocks = [Block 0 200, Block 50 200]

translateBlock :: Point -> Block -> Block
translateBlock (x, y) b = b {blockX = blockX b + x, blockY = blockY b + y}

createBlock :: Point -> Block
createBlock (x, y) = Block x y

-- NOTE: important constants

blockWidth :: Float
blockWidth = 50

blockHeight :: Float
blockHeight = 50
