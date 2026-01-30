module World (backgroundColor, startingWorld, drawGame, inputs, step, setMaps) where

import Block
import Damage
import Data.List (partition)
import Draw
import Enemy
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Item
import MapReader
import Player
import Shape
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
    initialiseGame (Menu m) = Game $ addMap (GameState startingPlayer [] [] [] 0 0 0 (mapsMenu m)) 0 (head $ mapsMenu m)

quitGameButton :: Button
quitGameButton = Button 0 (-200) 750 110 "Exit" (\_ -> error "quit game successfully") -- this is really naughty but realistically shouldn't be a problem

startingWorld :: State
startingWorld = Menu (MenuState 0 [startGameButton, quitGameButton] [])

setMaps :: State -> [Map] -> State
setMaps (Menu m) maps = Menu $ m {mapsMenu = maps}
setMaps (Game g) maps = Game $ g {maps = maps}

data MenuState = MenuState {selected :: Int, buttons :: [Button], mapsMenu :: [Map]}

data GameState = GameState
  { player :: Player,
    enemies :: [Enemy],
    blocks :: [Block],
    items :: [Item],
    score :: Int,
    treasures :: Int,
    depth :: Float,
    maps :: [Map]
  }

data State = Menu MenuState | Game GameState

-- this function requires more information that it necessarily should, so we can use various bits of data for the random seed
chooseRandomMap :: GameState -> Map
chooseRandomMap g
  | depth g + startingMapHeight + endingMapHeight >= maxDepth = last $ maps g
  | otherwise = maps g !! index
  where
    index = min (i + 1) (l - 2)
    i = round (depth g) `div` round mapHeight
    l = length $ maps g

step :: Float -> State -> State
step _ (Game g)
  | getHealth finalPlayer <= 0 = setMaps startingWorld (maps g) -- go back to the menu, not forgetting to inject maps back in
  | shouldLoadMap =
      Game $
        addMap updatedGame startingMapHeight (chooseRandomMap g)
  | otherwise = Game updatedGame
  where
    -- update player
    updatedGame =
      g
        { player = finalPlayer,
          enemies = finalEnemies,
          score = score g + enemyScore + itemScore,
          treasures = finalTreasures,
          blocks = movedBlocks,
          items = finalItems,
          depth = updatedDepth
        }
    (hit, damagedPlayer) = (playerDamage . playerMisc . playerMovement) (player g)
    movedPlayer = blockCollision damagedPlayer

    verticalChange =
      if depth g >= maxDepth
        then 0
        else descendingRate

    movedEnemies = map (blockCollision . enemyMovement (getCentre $ player g) . translateShape (0, verticalChange)) (enemies g)
    movedBlocks = map (translateShape (0, verticalChange)) (blocks g)
    movedItems = map (translateShape (0, verticalChange)) (items g)
    (enemyScore, finalEnemies) = enemyFilter . map (enemyMisc . enemyDamage) $ movedEnemies

    finalTreasures = max 0 $ treasures g + collectedTreasure + (if hit then (-1) else 0)

    updatedDepth :: Float
    updatedDepth = depth g + verticalChange

    -- do NOT use updated depth here, otherwise we skip first loading of map unless we set initial depth to (-1)

    shouldLoadMap :: Bool
    shouldLoadMap =
      floor (depth g / mapHeight)
        > floor ((depth g - verticalChange) / mapHeight)

    powerUpPlayer =
      if powerUpCollected
        then makeInvulnerable movedPlayer
        else movedPlayer

    anchorPlayer =
      if any (isAttacking movedPlayer) movedBlocks
        then fastRetract powerUpPlayer
        else powerUpPlayer

    finalPlayer = addHealth anchorPlayer collectedOxygen

    (collectedItems, finalItems) = partition (areIntersecting movedPlayer) movedItems

    (itemScore, collectedTreasure, collectedOxygen, powerUpCollected) = foldr f (0, 0, 0, False) collectedItems
      where
        f i (s, t, o, b) = case itemType i of
          Crystal -> (s + crystalScore, t, o, b)
          Treasure -> (s, t + 1, o, b)
          PowerUp -> (s + powerUpScore, t, o, True)
          OxygenTank -> (s, t, o + 1, True)

    blockCollision :: (Shape a) => a -> a
    blockCollision x = moveOutofBlocks x (intersectingBlocks x movedBlocks)

    playerDamage :: Player -> (Bool, Player)
    playerDamage p
      | any (areIntersecting p) (enemies g) = damage p
      | otherwise = (False, p)

    intersectingBlocks :: (Shape a) => a -> [Block] -> [Block]
    intersectingBlocks p = filter (areIntersecting p)

    moveOutofBlocks :: (Shape a) => a -> [Block] -> a
    moveOutofBlocks = foldr f
      where
        f :: (Shape a) => Block -> a -> a
        f b p = translateShape v p
          where
            v = findMTV b p

    enemyDamage :: Enemy -> Enemy
    enemyDamage e
      | isAttacking finalPlayer e = snd $ damage e
      | otherwise = e

    enemyFilter :: [Enemy] -> (Int, [Enemy])
    enemyFilter = enemyFilter' (0, [])
      where
        enemyFilter' acc [] = acc
        enemyFilter' (x, enemies) (e : es)
          | isAlive e = enemyFilter' (x, e : enemies) es
          | otherwise = enemyFilter' (x + getScore e, enemies) es
step _ s = s

addMap :: GameState -> Float -> Map -> GameState
addMap g offset m = g {enemies = enemies g ++ movedEnemies, blocks = blocks g ++ movedBlocks, items = items g ++ movedItems}
  where
    movedEnemies = map (translateShape (0, -offset)) (mapEnemies m)
    movedBlocks = map (translateShape (0, -offset)) (mapBlocks m)
    movedItems = map (translateShape (0, -offset)) (mapItems m)

backgroundColor :: Color
backgroundColor = black

drawGame :: SpriteSheet -> State -> Picture
-- draw game
drawGame ss (Game g) = Pictures $ drawBG : draw ss (player g) : map (draw ss) (enemies g) ++ map (draw ss) (blocks g) ++ map (draw ss) (items g) ++ [drawHUD]
  where
    drawBG :: Picture
    drawBG =
      Pictures
        [ Translate 0 (boundBackGround $ depth g / 3) $ Scale 2 2 $ backgroundSprite ss,
          Translate 0 (boundBackGround $ -backgroundSpriteHeight + depth g / 3) $ Scale 2 2 $ backgroundSprite ss
        ]

    boundBackGround :: Float -> Float
    boundBackGround x = fromIntegral $ (x' + (y `div` 2)) `mod` y - y `div` 2
      where
        x' = round x
        y = round $ screenHeight + backgroundSpriteHeight

    drawHUD :: Picture
    drawHUD =
      Pictures $
        [ Translate hudMiddle 0 $ Color hudColor $ rectangleSolid w screenHeight,
          Translate (-hudMiddle) 0 $ Color hudColor $ rectangleSolid w screenHeight,
          Translate 0 vMiddle $ Color hudColor $ rectangleSolid maxWidth v,
          Translate 0 (-vMiddle) $ Color hudColor $ rectangleSolid maxWidth v,
          Translate (-hudMiddle - 75) 75 $ Scale 0.5 0.5 $ Color white $ Text "Oxygen:",
          Translate (hudMiddle - 75) 275 $ Scale 0.5 0.5 $ Color white $ Text "Score:",
          Translate (hudMiddle - 75) 200 $ Scale 0.5 0.5 $ Color white $ Text $ printf "%05d" (score g),
          Translate (hudMiddle - 125) (-100) $ Scale 0.5 0.5 $ Color white $ Text "Treasure:"
          -- background blocks
        ]
          ++ [ Translate (-hudMiddle) 0 $
                 Translate (spacing * xOffset) (spacing * yOffset) $
                   Scale 4 4 $
                     oxygenSprite ss
               | x <- [0 .. hp - 1],
                 let xOffset = fromIntegral $ x `mod` 2,
                 let yOffset = fromIntegral $ -(x `div` 2)
             ]
          ++ [ Translate (hudMiddle - 100) (-150) $
                 Translate (spacing * xOffset) (spacing * yOffset) $
                   Scale 4 4 $
                     treasureSprite ss
               | x <- [0 .. treasures g - 1],
                 let xOffset = fromIntegral $ x `mod` 4,
                 let yOffset = fromIntegral $ -(x `div` 4)
             ]

    hp = getHealth $ player g
    w = (maxWidth - screenWidth) / 2
    v = (maxHeight - screenHeight) / 2
    hudMiddle = (screenWidth + w) / 2
    vMiddle = (screenHeight + v) / 2

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

crystalScore :: Int
crystalScore = 100

powerUpScore :: Int
powerUpScore = 1000

mapHeight :: Float
mapHeight = 2500

mapHeightInt :: Int
mapHeightInt = 2500

startingMapHeight :: Float
startingMapHeight = 1000

numMaps :: Float
numMaps = 3

endingMapHeight :: Float
endingMapHeight = 1000

maxDepth :: Float
maxDepth = startingMapHeight + numMaps * mapHeight + endingMapHeight - (screenHeight / 2)
