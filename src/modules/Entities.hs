module Entities (Player, Enemy, startingPlayer, startingEnemies, playerMovement, playerMisc, extend, directDown, directLeft, directUp, directRight, updateEnemyPos) where

import Draw
import Graphics.Gloss

-- NOTE: IMPORTANT CONSTANTS
playerWidth :: Float
playerWidth = 32

playerHeight :: Float
playerHeight = 28

jellyfishWidth :: Float
jellyfishWidth = 24

jellyfishHeight :: Float
jellyfishHeight = 45

sharkWidth :: Float
sharkWidth = 48

sharkHeight :: Float
sharkHeight = 15

data Anchor = Retracted | Extended Int Direction

instance Draw Anchor where
  draw _ Retracted = Blank
  draw _ (Extended x d) = Color blue $ Rotate (directionToAngle d) $ Translate ((length + playerWidth) / 2) 0 $ rectangleWire length 7
    where
      length = fromIntegral $ 15 * min (15 - abs (x - 15)) 12

data Player = Player {x :: Float, y :: Float, hp :: Int, direction :: Direction, anchor :: Anchor}

instance Draw Player where
  draw ss p = Translate (x p) (y p) $ Scale 2.5 2.5 $ Pictures [playerSprite ss, Color red $ rectangleWire playerWidth playerHeight, draw ss (anchor p)]

startingPlayer :: Player
startingPlayer = Player 0 0 0 (Direction 0 0) Retracted

extend :: Player -> Player
extend p = case anchor p of
  Retracted -> p {anchor = Extended 0 (direction p)}
  Extended _ _ -> p

-- player movement interface

playerMovement :: Player -> Player
playerMovement p =
  p {x = x p + horizontal * 7, y = y p + vertical * 7}
  where
    Direction h v = direction p
    horizontal = fromIntegral h
    vertical = fromIntegral v

playerMisc :: Player -> Player
playerMisc p = p {anchor = a}
  where
    a = case anchor p of
      Retracted -> Retracted
      Extended 30 _ -> Retracted
      Extended i d -> Extended (i + 1) d

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

data Shark = SharkData

data EnemyType = Shark | Jellyfish

data Enemy = Enemy {xPos :: Float, yPos :: Float, health :: Int, enemyType :: EnemyType}

updateEnemyPos :: Player -> Enemy -> Enemy
updateEnemyPos p e = case enemyType e of
  Shark -> e {xPos = xPos e + 3 * xdir, yPos = yPos e + 3 * ydir}
  Jellyfish -> e
  where
    xdir = signum $ x p - xPos e
    ydir = signum $ y p - yPos e

instance Draw Enemy where
  draw ss e = case enemyType e of
    Shark -> Translate (xPos e) (yPos e) $ Scale 3.5 3.5 $ Pictures [sharkSprite ss, Color red $ rectangleWire sharkWidth sharkHeight]
    Jellyfish -> Translate (xPos e) (yPos e) $ Scale 3.5 3.5 $ Pictures [jellyFishSprite ss, Color red $ rectangleWire jellyfishWidth jellyfishHeight]

startingEnemies :: [Enemy]
startingEnemies =
  [ Enemy 150 400 2 Shark,
    Enemy (-200) 200 1 Jellyfish
  ]
