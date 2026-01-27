module MapReader where

import Data.List.Split (splitOn)
import Entities

data Map = Map
  { mapEnemies :: [Enemy],
    mapItems :: [Item],
    mapBlocks :: [Block]
  }

data MapElement = Enemy Enemy | Item Item | Block Block | None

readMap :: String -> Map
readMap s = undefined
  where
    sSplit = zip [0 ..] $ lines s
    x = [parse x y c | (y, line) <- sSplit, (x, c) <- zip [0 ..] $ splitOn "," line]

    parse :: Int -> Int -> String -> MapElement
    parse x y c
      | c == "0" = None
      | otherwise = undefined
      where
        x' = x * 50 + 25
        y' = y * 50 + 25

    convert :: [MapElement] -> Map
    convert = undefined

-- todo: make getters for each type of item in map with an option to offset vertically by some amount
