module Entities (Player, Enemies, startingPlayer, startingEnemies, playerMovement, directDown, directLeft, directUp, directRight) where

import Draw
import Graphics.Gloss

data Player = Player {x :: Float, y :: Float, hp :: Int, direction :: Direction, moving :: Bool}

instance Draw Player where
  draw ss p = Translate (x p) (y p) $ Scale 2.5 2.5 $ playerSprite ss

startingPlayer :: Player
startingPlayer = Player 0 0 0 (Direction 0 0) True

playerMovement :: Player -> Player
playerMovement p
  | moving p =
      p {x = x p + horizontal * 7, y = y p + vertical * 7}
  | otherwise = p
  where
    Direction h v = direction p
    horizontal = fromIntegral h
    vertical = fromIntegral v

directDown :: Player -> Player
directDown p = p {direction = Direction h (v - 1)}
  where
    Direction h v = direction p

directUp :: Player -> Player
directUp p = p {direction = Direction h (v + 1)}
  where
    Direction h v = direction p

directLeft :: Player -> Player
directLeft p = p {direction = Direction (h - 1) v}
  where
    Direction h v = direction p

directRight :: Player -> Player
directRight p = p {direction = Direction (h + 1) v}
  where
    Direction h v = direction p

startingEnemies = [Shark, Shark, Jellyfish]

data Enemies = Shark | Jellyfish
