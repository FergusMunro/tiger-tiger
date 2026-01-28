module Item
  ( Item,
    ItemType (OxygenTank, Treasure, PowerUp, Crystal),
    startingItems,
    createItem,
  )
where

import Draw
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PointArithmetic
import Shape

data ItemType = OxygenTank | Treasure | PowerUp | Crystal

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
    Crystal -> Translate (itemX i) (itemY i) $ Scale 3 3 $ crystalSprite ss

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
  translateShape (x, y) i = i {itemX = itemX i + x, itemY = itemY i + y}

createItem :: ItemType -> Point -> Item
createItem t (x, y) = Item x y t

startingItems = [Item 0 (-200) Crystal]

-- NOTE: IMPORTANT CONSTANTS

itemWidth :: Float
itemWidth = 48

itemHeight :: Float
itemHeight = 48
