module Entities
  ( Player,
    Enemy,
    Block,
    Item,
    startingPlayer,
    startingEnemies,
    startingBlocks,
    playerMovement,
    playerMisc,
    extend,
    directDown,
    directLeft,
    directUp,
    directRight,
    updateEnemyPos,
    damagePlayer,
    enemyAttacked,
    damageEnemy,
    enemyMisc,
    enemyAlive,
    getScore,
    getHealth,
    EnemyType (Shark, Jellyfish),
    ItemType (OxygenTank, Treasure, PowerUp),
    createEnemy,
    createBlock,
    createItem,
    translateEnemy,
    translateBlock,
    translateItem,
  )
where

import Collision
import Draw
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PointArithmetic

-- player code

data Player = Player {x :: Float, y :: Float, hp :: Int, direction :: Direction, anchor :: Anchor, pDamageState :: DamageState}

instance Draw Player where
  draw ss p =
    Translate (x p) (y p) $
      Pictures [Scale 2.5 2.5 $ playerSprite ss, Color c $ rectangleWire playerWidth playerHeight, draw ss (anchor p)]
    where
      c = case pDamageState p of
        Vulnerable -> red
        Invulnerable _ -> blue

instance Shape Player where
  getCoordinates p =
    [ (x' - w, y' + h),
      (x' + w, y' + h),
      (x' + w, y' - h),
      (x' - w, y' - h)
    ]
    where
      x' = x p
      y' = y p
      w = playerWidth / 2
      h = playerHeight / 2

startingPlayer :: Player
startingPlayer = Player 0 0 2 (Direction 0 0) Retracted Vulnerable

getHealth :: Player -> Int
getHealth = hp

-- player movement interface

playerMovement :: Player -> Player
playerMovement p =
  p {x = boundX x', y = boundY y'}
  where
    x' = x p + horizontal * 7
    y' = y p + vertical * 7
    Direction h v = direction p
    horizontal = fromIntegral h
    vertical = fromIntegral v

    -- check screen bounds
    boundX :: Float -> Float
    boundX x
      | x - playerWidth / 2 < -(screenWidth / 2) = (playerWidth - screenWidth) / 2
      | x + playerWidth / 2 > screenWidth / 2 = (screenWidth - playerWidth) / 2
      | otherwise = x

    boundY :: Float -> Float
    boundY y
      | y - playerHeight / 2 < -(screenHeight / 2) = (playerHeight - screenHeight) / 2
      | y + playerHeight / 2 > (screenHeight / 2) = (screenHeight - playerHeight) / 2
      | otherwise = y

playerMisc :: Player -> Player
playerMisc p = p {anchor = a, pDamageState = updateDamageState (pDamageState p)}
  where
    a = case anchor p of
      Retracted -> Retracted
      Extended i d ->
        if i == anchorExtendedTime
          then Retracted
          else Extended (i + 1) d

-- Anchor Code

data Anchor = Retracted | Extended Int Direction

getAnchorLength :: (Num a) => Anchor -> a
getAnchorLength (Extended x _) = fromIntegral $ 35 * min (15 - abs (x - 15)) 12
getAnchorLength _ = 0

instance Draw Anchor where
  draw _ Retracted = Blank
  draw _ (Extended x d) =
    Color blue $
      Rotate angle $
        Translate ((length + playerWidth) / 2) 0 $
          rectangleWire length anchorHeight
    where
      length = getAnchorLength (Extended x d)
      -- for some unknown reason they rotate clockwise in their function so we need to do this
      angle = 360 - directionToAngle d

-- to draw the anchor we need to inject the coordinates, so that is what this type is for
data AnchorPos = AnchorPos Anchor Coordinate

instance Shape AnchorPos where
  getCoordinates (AnchorPos Retracted _) = []
  getCoordinates (AnchorPos (Extended l d) (x, y)) = map ((PointArithmetic.+ (x, y)) . rotate2d angle) coords
    where
      angle = directionToAngle d
      coords =
        [ (x' - w, y' + h),
          (x' + w, y' + h),
          (x' + w, y' - h),
          (x' - w, y' - h)
        ]
      w = getAnchorLength (Extended l d) / 2
      h = anchorHeight / 2
      x' = w + playerWidth / 2
      y' = 0

extend :: Player -> Player
extend p = case anchor p of
  Retracted -> p {anchor = Extended 0 (direction p)}
  Extended _ _ -> p

enemyAttacked :: Player -> Enemy -> Bool
enemyAttacked p e = case anchor p of
  Retracted -> False
  a -> areIntersecting (AnchorPos a (x p, y p)) e

-- damageCode
data DamageState = Vulnerable | Invulnerable Int

updateDamageState :: DamageState -> DamageState
updateDamageState (Invulnerable 0) = Vulnerable
updateDamageState (Invulnerable x) = Invulnerable (x - 1)
updateDamageState d = d

damagePlayer :: Player -> Player
damagePlayer p = case pDamageState p of
  Vulnerable -> p {hp = hp p - 1, pDamageState = Invulnerable playerDamageTime}
  Invulnerable _ -> p

damageEnemy :: Enemy -> Enemy
damageEnemy e = case eDamageState e of
  Vulnerable -> e {health = health e - 1, eDamageState = Invulnerable enemyDamageTime}
  Invulnerable _ -> e

enemyAlive :: Enemy -> Bool
enemyAlive e
  | health e <= 0 = False
  | otherwise = True

-- enemy Code

data EnemyType = Shark | Jellyfish

data Enemy = Enemy {xPos :: Float, yPos :: Float, health :: Int, enemyType :: EnemyType, eDamageState :: DamageState, aggroed :: Bool}

updateEnemyPos :: Player -> Enemy -> Enemy
updateEnemyPos p e = case enemyType e of
  Jellyfish -> e
  Shark ->
    if aggroed e || yDist < 400
      then e {xPos = xPos e + 3 * xdir, yPos = yPos e + 3 * ydir, aggroed = True}
      else e
  where
    yDist = y p - yPos e
    xdir = signum $ x p - xPos e
    ydir = signum yDist

enemyMisc :: Enemy -> Enemy
enemyMisc e = case eDamageState e of
  Vulnerable -> e
  Invulnerable 0 -> e {eDamageState = Vulnerable}
  Invulnerable x -> e {eDamageState = Invulnerable (x - 1)}

instance Draw Enemy where
  draw ss e = case enemyType e of
    Shark -> Translate (xPos e) (yPos e) $ Pictures [Scale 3 3 $ sharkSprite ss, Color c $ rectangleWire sharkWidth sharkHeight]
    Jellyfish -> Translate (xPos e) (yPos e) $ Pictures [Scale 3 3 $ jellyFishSprite ss, Color c $ rectangleWire jellyfishWidth jellyfishHeight]
    where
      c = case eDamageState e of
        Vulnerable -> red
        Invulnerable _ -> blue

instance Shape Enemy where
  getCoordinates e =
    [ (x' - w, y' + h),
      (x' + w, y' + h),
      (x' + w, y' - h),
      (x' - w, y' - h)
    ]
    where
      x' = xPos e
      y' = yPos e
      w = case enemyType e of
        Jellyfish -> jellyfishWidth / 2
        Shark -> sharkWidth / 2
      h = case enemyType e of
        Jellyfish -> jellyfishHeight / 2
        Shark -> sharkHeight / 2

getScore :: Enemy -> Int
getScore e = case enemyType e of
  Shark -> 800
  Jellyfish -> 500

-- block code

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

startingBlocks = [Block 0 200, Block 50 200]

-- item code

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

startingEnemies :: [Enemy]
startingEnemies =
  [ Enemy 150 (-400) 2 Shark Vulnerable False,
    Enemy (-200) 200 1 Jellyfish Vulnerable False
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

createEnemy :: EnemyType -> Point -> Enemy
createEnemy t (x, y) = Enemy x y h t Vulnerable False
  where
    h = case t of
      Shark -> 2
      Jellyfish -> 1

createBlock :: Point -> Block
createBlock (x, y) = Block x y

createItem :: ItemType -> Point -> Item
createItem t (x, y) = Item x y t

translatePlayer :: Point -> Player -> Player
translatePlayer (x', y') p = p {x = x p + x', y = y p + y'}

translateEnemy :: Point -> Enemy -> Enemy
translateEnemy (x, y) e = e {xPos = xPos e + x, yPos = yPos e + y}

translateBlock :: Point -> Block -> Block
translateBlock (x, y) b = b {blockX = blockX b + x, blockY = blockY b + y}

translateItem :: Point -> Item -> Item
translateItem (x, y) i = i {itemX = itemX i + x, itemY = itemY i + y}

-- data Enemy = Enemy {xPos :: Float, yPos :: Float, health :: Int, enemyType :: EnemyType, eDamageState :: DamageState, aggroed :: Bool}

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

playerDamageTime :: Int
playerDamageTime = 100

enemyDamageTime :: Int
enemyDamageTime = 30

anchorExtendedTime :: Int
anchorExtendedTime = 30

anchorHeight :: Float
anchorHeight = 20

blockWidth :: Float
blockWidth = 50

blockHeight :: Float
blockHeight = 50

itemWidth :: Float
itemWidth = 48

itemHeight :: Float
itemHeight = 48
