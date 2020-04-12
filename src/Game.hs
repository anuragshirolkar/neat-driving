module Game where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Renderer
import World
import Events

myDisplay = InWindow "Hello World" (1300, 700) (500, 500)

main :: IO ()
main = play myDisplay (light black) 60 initWorld renderWorld handleEvent actWorld