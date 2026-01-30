module Damage (DamageState (Vulnerable, Invulnerable), Damageable (damage, isAlive, getHealth, updateDamage, addHealth), updateDamageState) where

data DamageState = Vulnerable | Invulnerable Int

class Damageable a where
  damage :: a -> (Bool, a)
  isAlive :: a -> Bool
  getHealth :: a -> Int
  addHealth :: a -> Int -> a
  updateDamage :: a -> a

updateDamageState :: DamageState -> DamageState
updateDamageState (Invulnerable 0) = Vulnerable
updateDamageState (Invulnerable x) = Invulnerable (x - 1)
updateDamageState d = d
