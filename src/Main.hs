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
  player <- loadBMP "./resources/sprites/player.bmp"
  jellyFish <- loadBMP "./resources/sprites/jellyfish.bmp"
  shark <- loadBMP "./resources/sprites/shark.bmp"
  bg <- loadBMP "./resources/sprites/background.bmp"
  oxygen <- loadBMP "./resources/sprites/oxygen.bmp"
  treasure <- loadBMP "./resources/sprites/treasure.bmp"
  powerup <- loadBMP "./resources/sprites/powerup.bmp"
  crystal <- loadBMP "./resources/sprites/crystal.bmp"
  block <- loadBMP "./resources/sprites/block.bmp"

  map1 <- readFile "./resources/maps/1.csv"
  let mapList = [readMap map1]

  let ss = SpriteSheet player shark jellyFish bg oxygen treasure powerup crystal block
  play window backgroundColor steps startingWorld (drawGame ss) inputs (step mapList)
