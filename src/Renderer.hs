module Renderer where

import World
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Flow

renderCar :: Car -> Picture
renderCar c@Car{position=(px, py), orientation=deg} =
    Polygon [(w,l),(w,-l),(-w,-l),(-w,l)]
        |> Color red
        |> Rotate (orientation c)
        |> Translate px py
    where
        l = carLength/2
        w = carWidth/2

renderWorld :: World -> Picture
renderWorld w@World{car=car, track=track}
    | isCollision w     = Color blue $ Pictures [renderCar car, renderTrack track]
    | otherwise         = Pictures [renderCar car, renderTrack track]

renderTrack :: Track -> Picture
renderTrack (Track outer inner _) = Pictures [Line outer, Line inner]