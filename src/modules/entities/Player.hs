module Player
  ( Player,
    startingPlayer,
    playerMovement,
    playerMisc,
    isAttacking,
    extend,
    getAttackDirection,
    directDown,
    directLeft,
    directUp,
    directRight,
    fastRetract,
    makeInvulnerable,
  )
where

import Damage
import Draw
import Enemy
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PointArithmetic
import Shape

-- player code

data Player = Player {playerX :: Float, playerY :: Float, hp :: Int, direction :: Direction, anchor :: Anchor, pDamageState :: DamageState}

instance Draw Player where
  draw ss p =
    Translate (playerX p) (playerY p) $
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
      x' = playerX p
      y' = playerY p
      w = playerWidth / 2
      h = playerHeight / 2
  getCentre p = (playerX p, playerY p)
  translateShape (x, y) p = p {playerX = playerX p + x, playerY = playerY p + y}

instance Damageable Player where
  damage p = case pDamageState p of
    Vulnerable -> (True, p {hp = hp p - 1, pDamageState = Invulnerable playerDamageTime})
    Invulnerable _ -> (False, p)
  isAlive p = hp p > 0
  getHealth = hp
  updateDamage p = p {pDamageState = updateDamageState (pDamageState p)}
  addHealth p h = p {hp = hp p + h}

startingPlayer :: Player
startingPlayer = Player 0 0 4 (Direction 0 0) Retracted Vulnerable

-- player movement

playerMovement :: Player -> Player
playerMovement p =
  p {playerX = boundX x', playerY = boundY y'}
  where
    x' = playerX p + horizontal * 7
    y' = playerY p + vertical * 7
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

makeInvulnerable :: Player -> Player
makeInvulnerable p = p {pDamageState = Invulnerable powerUpTime}

-- Anchor Code

data Anchor = Retracted | Extended Int Direction

getAnchorLength :: (Num a) => Anchor -> a
getAnchorLength (Extended x _) = fromIntegral $ 20 * min (15 - abs (x - 15)) 12
getAnchorLength _ = 0

instance Draw Anchor where
  draw _ Retracted = Blank
  draw _ (Extended playerX d) =
    Color blue $
      Rotate angle $
        Translate ((length + playerWidth) / 2) 0 $
          rectangleWire length anchorHeight
    where
      length = getAnchorLength (Extended playerX d)
      -- for some unknown reason they rotate clockwise in their function so we need to do this
      angle = 360 - directionToAngle d

-- to draw the anchor we need to inject the coordinates, so that is what this type is for
data AnchorPos = AnchorPos Anchor Point

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
  getCentre (AnchorPos _ c) = c
  translateShape (x, y) (AnchorPos anchor (a, b)) = AnchorPos anchor (x + a, y + b)

extend :: Player -> Player
extend p = case anchor p of
  Retracted -> p {anchor = Extended 0 d}
  Extended _ _ -> p
  where
    d = case direction p of
      Direction 1 0 -> Direction 1 0
      Direction (-1) 0 -> Direction (-1) 0
      Direction _ x -> Direction 0 x

getAttackDirection :: Player -> Direction
getAttackDirection p = d
  where
    Extended _ d = anchor p

isAttacking :: (Shape a) => Player -> a -> Bool
isAttacking p e = case anchor p of
  Retracted -> False
  a -> areIntersecting (AnchorPos a (playerX p, playerY p)) e

fastRetract :: Player -> Player
fastRetract p = p {anchor = a}
  where
    a = case anchor p of
      Retracted -> Retracted
      Extended i d -> Extended (max i (anchorExtendedTime - i)) d

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

playerDamageTime :: Int
playerDamageTime = 100

anchorExtendedTime :: Int
anchorExtendedTime = 30

anchorHeight :: Float
anchorHeight = 20

powerUpTime :: Int
powerUpTime = 1000
