module MapReader where

import Block
import Data.List.Split (splitOn)
import Draw (screenWidth)
import Enemy
import Item
import Player

data Map = Map
  { mapEnemies :: [Enemy],
    mapItems :: [Item],
    mapBlocks :: [Block]
  }

data MapElement = Enemy Enemy | Item Item | Block Block | None

readMap :: String -> Map
readMap s = convert parsedText
  where
    sSplit = zip [0 ..] $ lines s
    parsedText = [parse x y c | (y, line) <- sSplit, (x, c) <- zip [0 ..] $ splitOn "," line]

    parse :: Int -> Int -> String -> MapElement
    parse x y c
      | c == "0" = None
      | c == "1" = Block $ createBlock (x', y')
      | c == "2" = Enemy $ createEnemy Shark (x', y')
      | c == "3" = Enemy $ createEnemy Jellyfish (x', y')
      | c == "4" = Item $ createItem OxygenTank (x', y')
      | c == "5" = Item $ createItem Treasure (x', y')
      | c == "6" = Item $ createItem PowerUp (x', y')
      | otherwise = error "invalid csv"
      where
        x' = fromIntegral x * 50 + 25 - (screenWidth / 2)
        y' = fromIntegral $ -(y * 50 + 25)

    convert :: [MapElement] -> Map
    convert xs = Map enemyList itemList blockList
      where
        (enemyList, itemList, blockList) = foldr f ([], [], []) xs
        f None xs = xs
        f (Enemy e) (es, is, bs) = (e : es, is, bs)
        f (Item i) (es, is, bs) = (es, i : is, bs)
        f (Block b) (es, is, bs) = (es, is, b : bs)

-- todo: make getters for each type of item in map with an option to offset vertically by some amount
