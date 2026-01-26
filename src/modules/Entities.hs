module Entities (Player, Enemy, startingPlayer, startingEnemies, playerMovement, playerMisc, extend, directDown, directLeft, directUp, directRight, updateEnemyPos, enemyCollision, damagePlayer) where

import Draw
import Graphics.Gloss

-- player code

data Player = Player {x :: Float, y :: Float, hp :: Int, direction :: Direction, anchor :: Anchor, pDamageState :: DamageState}

instance Draw Player where
  draw ss p = case pDamageState p of
    Invulnerable _ ->
      Translate (x p) (y p) $
        Pictures [Scale 2.5 2.5 $ playerSprite ss, Color blue $ rectangleWire playerWidth playerHeight, Scale 2.5 2.5 $ draw ss (anchor p)]
    Vulnerable ->
      Translate (x p) (y p) $
        Pictures [Scale 2.5 2.5 $ playerSprite ss, Color red $ rectangleWire playerWidth playerHeight, Scale 2.5 2.5 $ draw ss (anchor p)]

startingPlayer :: Player
startingPlayer = Player 0 0 0 (Direction 0 0) Retracted Vulnerable

-- player movement interface

playerMovement :: Player -> Player
playerMovement p =
  p {x = x p + horizontal * 7, y = y p + vertical * 7}
  where
    Direction h v = direction p
    horizontal = fromIntegral h
    vertical = fromIntegral v

playerMisc :: Player -> Player
playerMisc p = p {anchor = a, pDamageState = updateDamageState (pDamageState p)}
  where
    a = case anchor p of
      Retracted -> Retracted
      Extended 30 _ -> Retracted
      Extended i d -> Extended (i + 1) d

-- Anchor Code

data Anchor = Retracted | Extended Int Direction

instance Draw Anchor where
  draw _ Retracted = Blank
  draw _ (Extended x d) = Color blue $ Rotate (directionToAngle d) $ Translate ((length + playerWidth) / 2) 0 $ rectangleWire length 7
    where
      length = fromIntegral $ 15 * min (15 - abs (x - 15)) 12

extend :: Player -> Player
extend p = case anchor p of
  Retracted -> p {anchor = Extended 0 (direction p)}
  Extended _ _ -> p

-- damageCode
data DamageState = Vulnerable | Invulnerable Int

updateDamageState :: DamageState -> DamageState
updateDamageState (Invulnerable 0) = Vulnerable
updateDamageState (Invulnerable x) = Invulnerable (x - 1)
updateDamageState d = d

damagePlayer :: Player -> Player
damagePlayer p = p {hp = hp p - 1, pDamageState = Invulnerable playerDamageTime}

-- enemy Code

data EnemyType = Shark | Jellyfish

data Enemy = Enemy {xPos :: Float, yPos :: Float, health :: Int, enemyType :: EnemyType, eDamageState :: DamageState}

updateEnemyPos :: Player -> Enemy -> Enemy
updateEnemyPos p e = case enemyType e of
  Shark -> e {xPos = xPos e + 3 * xdir, yPos = yPos e + 3 * ydir}
  Jellyfish -> e
  where
    xdir = signum $ x p - xPos e
    ydir = signum $ y p - yPos e

instance Draw Enemy where
  draw ss e = case enemyType e of
    Shark -> Translate (xPos e) (yPos e) $ Pictures [Scale 3 3 $ sharkSprite ss, Color red $ rectangleWire sharkWidth sharkHeight]
    Jellyfish -> Translate (xPos e) (yPos e) $ Pictures [Scale 3 3 $ jellyFishSprite ss, Color red $ rectangleWire jellyfishWidth jellyfishHeight]

enemyCollision :: Player -> Enemy -> Bool
enemyCollision p e = case pDamageState p of
  Vulnerable ->
    case enemyType e of
      Shark ->
        (xdiff <= (playerWidth + sharkWidth) / 2)
          && (ydiff <= (playerHeight + sharkHeight) / 2)
      Jellyfish ->
        (xdiff <= (playerWidth + jellyfishWidth) / 2)
          && (ydiff <= (playerHeight + jellyfishHeight) / 2)
    where
      xdiff = abs $ x p - xPos e
      ydiff = abs $ y p - yPos e
  _ -> False

startingEnemies :: [Enemy]
startingEnemies =
  [ Enemy 150 400 2 Shark Vulnerable,
    Enemy (-200) 200 1 Jellyfish Vulnerable
  ]

-- player direction interfaces
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

-- NOTE: IMPORTANT CONSTANTS
playerWidth :: Float
playerWidth = 80

playerHeight :: Float
playerHeight = 70

jellyfishWidth :: Float
jellyfishWidth = 72

jellyfishHeight :: Float
jellyfishHeight = 135

sharkWidth :: Float
sharkWidth = 144

sharkHeight :: Float
sharkHeight = 45

playerDamageTime = 100
