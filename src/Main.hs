module Main where

import Draw
import Graphics.Gloss
import World

steps :: Int -- steps per second
steps = 60

window :: Display
window = FullScreen

main :: IO ()
main = do
  player <- loadBMP "./resources/player.bmp"

  let ss = SpriteSheet player
  play window background steps startingWorld (drawGame ss) inputs step
