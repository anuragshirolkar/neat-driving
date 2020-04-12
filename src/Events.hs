module Events where

import Graphics.Gloss.Interface.IO.Interact
import World


handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) = startGas
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) = stopGas
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) = startBrake
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) = stopBrake

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) = startTurnLeft
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) = stopTurnLeft
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = startTurnRight
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) = stopTurnRight

handleEvent _ = id
