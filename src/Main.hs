module Main where

import Draw
import Graphics.Gloss
import MapReader
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
  bg <- loadBMP "./resources/background.bmp"
  oxygen <- loadBMP "./resources/oxygen.bmp"
  treasure <- loadBMP "./resources/treasure.bmp"
  powerup <- loadBMP "./resources/powerup.bmp"
  block <- loadBMP "./resources/block.bmp"

  map1 <- readFile "./resources/maps/1.csv"
  let mapList = [readMap map1]

  let ss = SpriteSheet player shark jellyFish bg oxygen treasure powerup block
  play window backgroundColor steps startingWorld (drawGame ss) inputs (step mapList)
