module Item
  ( Item,
    ItemType (OxygenTank, Treasure, PowerUp),
    startingItems,
    createItem,
    translateItem,
  )
where

import Draw
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PointArithmetic
import Shape

data ItemType = OxygenTank | Treasure | PowerUp

data Item = Item
  { itemX :: Float,
    itemY :: Float,
    itemType :: ItemType
  }

instance Draw Item where
  draw ss i = case itemType i of
    OxygenTank -> Translate (itemX i) (itemY i) $ Scale 3 3 $ oxygenSprite ss
    Treasure -> Translate (itemX i) (itemY i) $ Scale 3 3 $ treasureSprite ss
    PowerUp -> Translate (itemX i) (itemY i) $ Scale 3 3 $ powerUpSprite ss

instance Shape Item where
  getCoordinates i =
    [ (x' - w, y' + h),
      (x' + w, y' + h),
      (x' + w, y' - h),
      (x' - w, y' - h)
    ]
    where
      x' = itemX i
      y' = itemY i
      w = itemWidth / 2
      h = itemHeight / 2

  getCentre i = (itemX i, itemY i)

createItem :: ItemType -> Point -> Item
createItem t (x, y) = Item x y t

translateItem :: Point -> Item -> Item
translateItem (x, y) i = i {itemX = itemX i + x, itemY = itemY i + y}

startingItems = []

-- NOTE: IMPORTANT CONSTANTS

itemWidth :: Float
itemWidth = 48

itemHeight :: Float
itemHeight = 48
