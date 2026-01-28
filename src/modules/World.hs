module World (backgroundColor, startingWorld, drawGame, inputs, step) where

import Collision
import Draw
import Entities
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import MapReader
import Text.Printf

data Button = Button {x :: Float, y :: Float, width :: Float, height :: Float, message :: String, effect :: State -> State}

instance Draw Button where
  draw ss b =
    Pictures
      [ Translate (x b) (y b) $ rectangleSolid (width b) (height b),
        Color green $ Translate (x b - width b / 2) (y b - height b / 2 + 5) (Text (message b))
      ]

startGameButton :: Button
startGameButton = Button 0 0 750 110 "Start Game" initialiseGame
  where
    initialiseGame _ = Game $ GameState startingPlayer startingEnemies startingBlocks [] 0 0 0

quitGameButton :: Button
quitGameButton = Button 0 (-200) 750 110 "Exit" (\_ -> error "quit game successfully") -- this is really naughty but realistically shouldn't be a problem

startingWorld :: State
startingWorld = Menu (MenuState 0 [startGameButton, quitGameButton])

data MenuState = MenuState {selected :: Int, buttons :: [Button]}

data GameState = GameState
  { player :: Player,
    enemies :: [Enemy],
    blocks :: [Block],
    items :: [Item],
    score :: Int,
    treasures :: Int,
    depth :: Float
  }

data State = Menu MenuState | Game GameState

step :: [Map] -> Float -> State -> State
step maps _ (Game g)
  | getHealth finalPlayer <= 0 = startingWorld
  | otherwise =
      Game $
        g
          { player = finalPlayer,
            enemies = finalEnemies,
            score = finalScore + score g,
            blocks = movedBlocks,
            items = movedItems,
            depth = depth g + descendingRate
          }
  where
    -- update player
    finalPlayer = (playerDamage . playerMisc . playerMovement) (player g)
    -- handle anchor collisions
    movedEnemies = map (updateEnemyPos (player g) . translateEnemy (0, descendingRate)) (enemies g)
    movedBlocks = map (translateBlock (0, descendingRate)) (blocks g)
    movedItems = map (translateItem (0, descendingRate)) (items g)
    (finalScore, finalEnemies) = enemyFilter . map (enemyMisc . enemyDamage) $ movedEnemies

    playerDamage :: Player -> Player
    playerDamage p
      | any (areIntersecting p) (enemies g) = damagePlayer p
      | otherwise = p

    enemyDamage :: Enemy -> Enemy
    enemyDamage e
      | enemyAttacked finalPlayer e = damageEnemy e
      | otherwise = e

    enemyFilter :: [Enemy] -> (Int, [Enemy])
    enemyFilter = enemyFilter' (0, [])
      where
        enemyFilter' acc [] = acc
        enemyFilter' (x, enemies) (e : es)
          | enemyAlive e = enemyFilter' (x, e : enemies) es
          | otherwise = enemyFilter' (x + getScore e, enemies) es
step maps _ s = Game $ addMap (GameState startingPlayer [] [] [] 0 0 0) 300 (head maps)

addMap :: GameState -> Float -> Map -> GameState
addMap g offset m = g {enemies = enemies g ++ movedEnemies, blocks = blocks g ++ movedBlocks, items = items g ++ movedItems}
  where
    movedEnemies = map (translateEnemy (0, -offset)) (mapEnemies m)
    movedBlocks = map (translateBlock (0, -offset)) (mapBlocks m)
    movedItems = map (translateItem (0, -offset)) (mapItems m)

backgroundColor :: Color
backgroundColor = black

drawGame :: SpriteSheet -> State -> Picture
-- draw game
drawGame ss (Game g) = Pictures $ drawBG : drawHUD : draw ss (player g) : map (draw ss) (enemies g) ++ map (draw ss) (blocks g) ++ map (draw ss) (items g)
  where
    drawBG :: Picture
    drawBG =
      Pictures
        [ Translate 0 (boundBackGround $ depth g / 3) $ Scale 2 2 $ backgroundSprite ss,
          Translate 0 (boundBackGround $ -backgroundSpriteHeight + depth g / 3) $ Scale 2 2 $ backgroundSprite ss,
          Translate hudMiddle 0 $ Color hudColor $ rectangleSolid w screenHeight,
          Translate (-hudMiddle) 0 $ Color hudColor $ rectangleSolid w screenHeight
        ]

    boundBackGround :: Float -> Float
    boundBackGround x = fromIntegral $ (x' + (y `div` 2)) `mod` y - y `div` 2
      where
        x' = round x
        y = round $ screenHeight + backgroundSpriteHeight

    drawHUD :: Picture
    drawHUD =
      Pictures $
        [ Translate (-hudMiddle - 75) 75 $ Scale 0.5 0.5 $ Color white $ Text "Oxygen:",
          Translate (hudMiddle - 75) 75 $ Scale 0.5 0.5 $ Color white $ Text "Score:",
          Translate (hudMiddle - 75) 0 $ Scale 0.5 0.5 $ Color white $ Text $ printf "%05d" (score g)
        ]
          ++ [ Translate (-hudMiddle) 0 $
                 Translate (spacing * xOffset) (spacing * yOffset) $
                   Scale 4 4 $
                     oxygenSprite ss
               | x <- [0 .. hp - 1],
                 let xOffset = fromIntegral $ x `mod` 2,
                 let yOffset = fromIntegral $ -(x `div` 2)
             ]

    hp = getHealth $ player g
    w = (maxWidth - screenWidth) / 2
    hudMiddle = (screenWidth + w) / 2

    spacing = 16 * 4 + 10

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

-- NOTE: important constants
--
descendingRate :: Float
descendingRate = 1
