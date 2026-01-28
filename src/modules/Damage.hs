module Damage (DamageState (Vulnerable, Invulnerable), Damageable (damage, isAlive, getHealth, updateDamage), updateDamageState) where

data DamageState = Vulnerable | Invulnerable Int

class Damageable a where
  damage :: a -> a
  isAlive :: a -> Bool
  getHealth :: a -> Int
  updateDamage :: a -> a

updateDamageState :: DamageState -> DamageState
updateDamageState (Invulnerable 0) = Vulnerable
updateDamageState (Invulnerable x) = Invulnerable (x - 1)
updateDamageState d = d
