module World where

import Graphics.Gloss.Data.Point
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Data.Maybe as Mb
import Flow
import Brain


{-|
    CAR
-}
data Car =
    Car { position :: Point
        , orientation :: Float -- in degrees
        , turn :: Float
        , velocity :: Float
        , acceleration :: Float
        , brain :: Brain
        }

carLength = 40.0 :: Float
carWidth = 20.0 :: Float

carPath :: Car -> Path
carPath Car{position=pos, orientation=o} =
    initBox
        |> map (rotateV orientationInRad)
        |> map (pos PA.+)
    where
        l = carLength/2
        w = carWidth/2
        initBox = [(w,l),(w,-l),(-w,-l),(-w,l),(w,l)]
        orientationInRad = negate (degToRad o)

class ControlCar a where
    startGas :: a -> a
    stopGas :: a -> a
    startBrake :: a -> a
    stopBrake :: a -> a

    startTurnLeft :: a -> a
    stopTurnLeft :: a -> a
    startTurnRight :: a -> a
    stopTurnRight :: a -> a

    step :: Float -> a -> a

instance ControlCar Car where
    startGas car    = car { velocity = 200.0 }
    stopGas car     = car { velocity = 0.0 }
    startBrake car  = car { velocity = -200.0 }
    stopBrake car   = car { velocity = 0.0 }

    startTurnLeft car   = car { turn = -1}
    stopTurnLeft car    = car { turn = 0}
    startTurnRight car  = car { turn = 1}
    stopTurnRight car   = car { turn = 0}

    step dt car@(Car (x,y) o t v _ b) =
        car {
            position = (x+dt*v*sin (degToRad o), y+dt*v*cos (degToRad o)),
            orientation = o+100*t*dt
        }

actCar :: Float -> Sensors -> Car -> Car
actCar dt s car =
    car
        |> (if ma > 0.5 then startGas else id)
        |> (if ta > 0.5 then startTurnRight else if ta < (-0.5) then startTurnLeft else id)
        |> step dt
    where
        Actions ma ta = think (brain car) s


{-|
    WORLD
-}
data World =
    World { car :: Car
          , track :: Track
          }

instance ControlCar World where
    startGas w@World{car=car}   = w { car = startGas car}
    stopGas w@World{car=car}    = w { car = stopGas car}
    startBrake w@World{car=car} = w { car = startBrake car}
    stopBrake w@World{car=car}  = w { car = stopBrake car}

    startTurnLeft w@World{car=car}  = w {car = startTurnLeft car}
    stopTurnLeft w@World{car=car}   = w {car = stopTurnLeft car}
    startTurnRight w@World{car=car} = w {car = startTurnRight car}
    stopTurnRight w@World{car=car}  = w {car = stopTurnRight car}

    step dt w@World{car=car}    = w { car = step dt car}

sense :: World -> Sensors
sense _ = Sensors 0

actWorld :: Float -> World -> World
actWorld dt w =
    w { car = actCar dt sensorIn $ car w}
    where
        sensorIn = sense w

initWorld :: World
initWorld =
    World (Car (-250, -100) 0.0 0.0 0.0 0.0 (Brain 0)) initTrack

{-|
    TRACK
-}
data Track =
    Track { outer :: Path
          , inner :: Path
          }

initTrack :: Track
initTrack =
    Track
        [ (300,300), (300,-300), (-300,-300), (-300,300), (300,300) ]
        [ (200,200), (200,-200), (-200,-200), (-200,200), (200,200) ]


{-|
    COLLISION DETECTION
    If equation of a line is f.(x,y) = ax + by + c = 0, the car has collided
    with the line iff (f.p1 * f.p2) <= 0 or (f.q1 * f.q2) <= 0. (p1,p2) and
    (q1,q2) are pairs of opposite corners of the car.
-}
isCollision :: World -> Bool
isCollision (World car track) =
    pathsIntersect cPath (inner track) || pathsIntersect cPath (outer track)
    where
        cPath = carPath car

pathsIntersect :: Path -> Path -> Bool
pathsIntersect [] _ = False
pathsIntersect [_] _ = False
pathsIntersect (t:u:vs) path
    | segmentPathIntersect (t,u) path   = True
    | otherwise                         = pathsIntersect (u:vs) path

segmentPathIntersect :: (Point, Point) -> Path -> Bool
segmentPathIntersect seg [] = False
segmentPathIntersect seg [_] = False
segmentPathIntersect (q1,q2) (p1:p2:ps)
    | Mb.isJust $ intersectSegSeg q1 q2 p1 p2 = True
    | otherwise                     = segmentPathIntersect (q1,q2) (p2:ps)
