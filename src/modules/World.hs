module World (background, startingWorld, drawGame, inputs, step) where

import Collision
import Draw
import Entities
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

data Button = Button {x :: Float, y :: Float, width :: Float, height :: Float, message :: String, effect :: State -> State}

instance Draw Button where
  draw ss b =
    Pictures
      [ Translate (x b) (y b) $ rectangleSolid (width b) (height b),
        Color green $ Translate (x b - width b / 2) (y b - height b / 2 + 5) (Text (message b))
      ]

startGameButton :: Button
startGameButton = Button 0 0 750 110 "Start Game" foo
  where
    foo (Menu m) = Game $ GameState startingPlayer startingEnemies False

quitGameButton :: Button
quitGameButton = Button 0 (-200) 750 110 "Exit" (\_ -> error "quit game successfully") -- this is really naughty but realistically shouldn't be a problem

startingWorld :: State
startingWorld = Menu (MenuState 0 [startGameButton, quitGameButton])

data MenuState = MenuState {selected :: Int, buttons :: [Button]}

data GameState = GameState
  { player :: Player,
    enemies :: [Enemy],
    paused :: Bool
  }

data State = Menu MenuState | Game GameState

background :: Color
background = black

drawGame :: SpriteSheet -> State -> Picture
-- draw game
drawGame ss (Game g) = Pictures $ draw ss (player g) : map (draw ss) (enemies g)
-- draw menu
drawGame ss (Menu m) = drawnButtons
  where
    drawnButtons = Pictures (drawButtons (selected m) (buttons m))

    drawButtons :: Int -> [Button] -> [Picture]
    drawButtons 0 (b : bs) = Color red (draw ss b) : drawButtons (-1) bs
    drawButtons i (b : bs) = Color white (draw ss b) : drawButtons (i - 1) bs
    drawButtons _ [] = []

inputs :: Event -> State -> State
---- game events -----------------------
-- directional keys down
inputs (EventKey (Char 'w') Down _ _) (Game g) = Game $ g {player = directUp (player g)}
inputs (EventKey (Char 's') Down _ _) (Game g) = Game $ g {player = directDown (player g)}
inputs (EventKey (Char 'a') Down _ _) (Game g) = Game $ g {player = directLeft (player g)}
inputs (EventKey (Char 'd') Down _ _) (Game g) = Game $ g {player = directRight (player g)}
inputs (EventKey (SpecialKey KeySpace) Down _ _) (Game g) = Game $ g {player = extend (player g)}
-- directional keys up
inputs (EventKey (Char 'w') Up _ _) (Game g) = Game $ g {player = directDown (player g)}
inputs (EventKey (Char 's') Up _ _) (Game g) = Game $ g {player = directUp (player g)}
inputs (EventKey (Char 'a') Up _ _) (Game g) = Game $ g {player = directRight (player g)}
inputs (EventKey (Char 'd') Up _ _) (Game g) = Game $ g {player = directLeft (player g)}
---- menu events -----------------------
inputs (EventKey (Char 's') Down _ _) (Menu m) = Menu $ m {selected = (selected m + 1) `mod` length (buttons m)}
inputs (EventKey (Char 'w') Down _ _) (Menu m) = Menu $ m {selected = (selected m - 1) `mod` length (buttons m)}
inputs (EventKey (SpecialKey KeyEnter) Down _ _) (Menu m) = effect pressed (Menu m)
  where
    pressed = buttons m !! selected m
inputs _ s = s

step :: Float -> State -> State
step _ (Game g) =
  Game $
    g
      { player = finalPlayer,
        enemies = finalEnemies
      }
  where
    -- update player
    movedPlayer = (playerMisc . playerMovement) (player g)
    playerDamaged = any (areIntersecting movedPlayer) (enemies g)
    finalPlayer =
      if playerDamaged
        then damagePlayer movedPlayer
        else movedPlayer
    -- handle anchor collisions
    movedEnemies = map (updateEnemyPos (player g)) (enemies g)
    finalEnemies = map f movedEnemies
    f e
      | enemyAttacked finalPlayer e = damageEnemy e
      | otherwise = e
step _ s = s
