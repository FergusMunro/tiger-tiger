module Enemy
  ( Enemy,
    EnemyType (Shark, Jellyfish),
    enemyMovement,
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

data EnemyType = Shark | Jellyfish

data Enemy = Enemy {enemyX :: Float, enemyY :: Float, health :: Int, enemyType :: EnemyType, eDamageState :: DamageState, aggroed :: Bool}

instance Draw Enemy where
  draw ss e = case enemyType e of
    Shark -> Translate (enemyX e) (enemyY e) $ Pictures [Scale 3 3 $ sharkSprite ss, Color c $ rectangleWire sharkWidth sharkHeight]
    Jellyfish -> Translate (enemyX e) (enemyY e) $ Pictures [Scale 3 3 $ jellyFishSprite ss, Color c $ rectangleWire jellyfishWidth jellyfishHeight]
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
      x' = enemyX e
      y' = enemyY e
      w = case enemyType e of
        Jellyfish -> jellyfishWidth / 2
        Shark -> sharkWidth / 2
      h = case enemyType e of
        Jellyfish -> jellyfishHeight / 2
        Shark -> sharkHeight / 2
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

enemyMovement :: PlayerPos -> Enemy -> Enemy
enemyMovement (x, y) e = case enemyType e of
  Jellyfish -> e
  Shark ->
    if aggroed e || yDist < 400
      then e {enemyX = enemyX e + 3 * xdir, enemyY = enemyY e + 3 * ydir, aggroed = True}
      else e
  where
    yDist = y - enemyY e
    xdir = signum $ x - enemyX e
    ydir = signum yDist

enemyMisc :: Enemy -> Enemy
enemyMisc e = case eDamageState e of
  Vulnerable -> e
  Invulnerable 0 -> e {eDamageState = Vulnerable}
  Invulnerable x -> e {eDamageState = Invulnerable (x - 1)}

getScore :: Enemy -> Int
getScore e = case enemyType e of
  Shark -> 800
  Jellyfish -> 500

createEnemy :: EnemyType -> Point -> Enemy
createEnemy t (x, y) = Enemy x y h t Vulnerable False
  where
    h = case t of
      Shark -> 2
      Jellyfish -> 1

-- NOTE: IMPORTANT CONSTANTS

jellyfishWidth :: Float
jellyfishWidth = 72

jellyfishHeight :: Float
jellyfishHeight = 135

sharkWidth :: Float
sharkWidth = 144

sharkHeight :: Float
sharkHeight = 45

enemyDamageTime :: Int
enemyDamageTime = 30
