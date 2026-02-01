module Item
  ( Item,
    itemType,
    ItemType (OxygenTank, Treasure, PowerUp, Crystal, BigChest),
    createItem,
  )
where

import Draw
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PointArithmetic
import Shape

data ItemType = OxygenTank | Treasure | PowerUp | Crystal | BigChest

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
    BigChest -> Translate (itemX i) (itemY i) $ Scale 3 3 $ bigTreasureSprite ss

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

      w = case itemType i of
        BigChest -> bigChestWidth / 2
        _ -> itemWidth / 2

      h = case itemType i of
        BigChest -> bigChestHeight / 2
        _ -> itemHeight / 2

  getCentre i = (itemX i, itemY i)
  translateShape (x, y) i = i {itemX = itemX i + x, itemY = itemY i + y}

createItem :: ItemType -> Point -> Item
createItem t (x, y) = case t of
  BigChest -> Item x (y + 20) t -- centre the chest on same grid as tiles
  _ -> Item x y t

-- NOTE: IMPORTANT CONSTANTS

itemWidth :: Float
itemWidth = 48

itemHeight :: Float
itemHeight = 48

bigChestWidth :: Float
bigChestWidth = 96

bigChestHeight :: Float
bigChestHeight = 96
