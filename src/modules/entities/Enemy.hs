module Enemy
  ( Enemy,
    EnemyType (Shark, Jellyfish, Turtle, RedShark),
    enemyMovement,
    isBlocking,
    enemyMisc,
    getScore,
    createEnemy,
  )
where

import Damage
import Draw
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PointArithmetic
import Shape

data EnemyType = Shark | Jellyfish | Turtle | RedShark

data Enemy = Enemy {enemyX :: Float, enemyY :: Float, health :: Int, enemyType :: EnemyType, eDamageState :: DamageState, aggroed :: Bool, enemyTimer :: Int}

instance Draw Enemy where
  draw ss e = case enemyType e of
    Jellyfish -> Translate (enemyX e) (enemyY e) $ Pictures [Scale 3 3 $ jellyFishSprites ss !! spriteIndex, Color c $ rectangleWire jellyfishWidth jellyfishHeight]
    Turtle -> Translate (enemyX e) (enemyY e) $ Scale (-getDirection e) 1 $ Pictures [Scale 3 3 $ turtleSprites ss !! spriteIndex, Color c $ rectangleWire turtleWidth turtleHeight]
    Shark -> Translate (enemyX e) (enemyY e) $ Scale (-getDirection e) 1 $ Pictures [Scale 3 3 $ sharkSprites ss !! spriteIndex, Color c $ rectangleWire sharkWidth sharkHeight]
    RedShark -> Translate (enemyX e) (enemyY e) $ Scale (-getDirection e) 1 $ Pictures [Scale 3 3 $ redSharkSprites ss !! spriteIndex, Color c $ rectangleWire sharkWidth sharkHeight]
    where
      c = case eDamageState e of
        Vulnerable -> red
        Invulnerable _ -> blue

      spriteIndex :: Int
      spriteIndex = case enemyType e of
        Jellyfish -> (enemyTimer e `div` jellyAnimationLength) `mod` 2
        Turtle -> (enemyTimer e `div` turtleAnimationLength) `mod` 2
        _ -> (enemyTimer e `div` sharkAnimationLength) `mod` 2

instance Shape Enemy where
  getCoordinates e =
    [ (x' - w, y' + h),
      (x' + w, y' + h),
      (x' + w, y' - h),
      (x' - w, y' - h)
    ]
    where
      x' = enemyX e
      y' = enemyY e
      w = case enemyType e of
        Jellyfish -> jellyfishWidth / 2
        Turtle -> turtleWidth / 2
        _ -> sharkWidth / 2

      h = case enemyType e of
        Jellyfish -> jellyfishHeight / 2
        Turtle -> turtleHeight / 2
        _ -> sharkHeight / 2
  getCentre e = (enemyX e, enemyY e)
  translateShape (x, y) e = e {enemyX = enemyX e + x, enemyY = enemyY e + y}

instance Damageable Enemy where
  damage e = case eDamageState e of
    Vulnerable -> (True, e {health = health e - 1, eDamageState = Invulnerable enemyDamageTime})
    Invulnerable _ -> (False, e)
  isAlive e = health e > 0
  getHealth = health
  updateDamage e = e {eDamageState = updateDamageState (eDamageState e)}
  addHealth e h = e {health = health e + h}

type PlayerPos = Point

isBlocking :: Direction -> Enemy -> Bool
isBlocking d e =
  case enemyType e of
    Jellyfish ->
      case d of
        Direction 0 (-1) -> False
        _ -> True
    Turtle ->
      case d of
        Direction 0 (1) -> False
        _ -> True
    _ -> False

enemyMovement :: PlayerPos -> Enemy -> Enemy
enemyMovement (x, y) e = case enemyType e of
  Jellyfish -> e
  RedShark ->
    if aggroed e || yDist < 400
      then e {enemyX = enemyX e + 3 * xdir, enemyY = enemyY e + 3 * ydir, aggroed = True}
      else e {enemyX = enemyX e + sharkMoveSpeed * getDirection e}
  _ ->
    e {enemyX = enemyX e + sharkMoveSpeed * getDirection e}
  where
    yDist = y - enemyY e
    xdir = signum $ x - enemyX e
    ydir = signum yDist

getDirection :: Enemy -> Float
getDirection e = case enemyType e of
  Jellyfish -> 0
  Shark -> fromIntegral $ (((enemyTimer e `div` sharkMoveTime) `mod` 2) * 2) - 1
  Turtle -> fromIntegral $ (((enemyTimer e `div` turtleMoveTime) `mod` 2) * 2) - 1
  RedShark ->
    if aggroed e
      then 1 -- always faces left when aggroed because i'm lazy
      else fromIntegral $ (((enemyTimer e `div` sharkMoveTime) `mod` 2) * 2) - 1

enemyMisc :: Enemy -> Enemy
enemyMisc e = updateDamage $ e {enemyTimer = enemyTimer e + 1}

getScore :: Enemy -> Int
getScore e = case enemyType e of
  Shark -> 800
  Turtle -> 800
  Jellyfish -> 500
  RedShark -> 3000

createEnemy :: EnemyType -> Point -> Enemy
createEnemy t (x, y) = Enemy x y h t Vulnerable False 0
  where
    h = case t of
      RedShark -> 3
      _ -> 1

-- NOTE: IMPORTANT CONSTANTS

jellyfishWidth :: Float
jellyfishWidth = 72

jellyfishHeight :: Float
jellyfishHeight = 135

sharkWidth :: Float
sharkWidth = 144

sharkHeight :: Float
sharkHeight = 45

turtleWidth :: Float
turtleWidth = 117

turtleHeight :: Float
turtleHeight = 48

sharkMoveTime :: Int
sharkMoveTime = 150

turtleMoveTime :: Int
turtleMoveTime = 120

sharkMoveSpeed :: Float
sharkMoveSpeed = 2

enemyDamageTime :: Int
enemyDamageTime = 30
