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
  jellyFish <- loadBMP "./resources/jellyfish.bmp"
  shark <- loadBMP "./resources/shark.bmp"

  let ss = SpriteSheet player shark jellyFish
  play window background steps startingWorld (drawGame ss) inputs step
