module Util where

import qualified Data.Maybe as Mb
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Data.Vector

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

isInsidePath :: Path -> Point -> Bool
isInsidePath path pt = odd (countIntersections path) 
    where
        isInvalidAnchor ap = any (liesOnLine pt ap) path
        getAnchor (x,y) = if isInvalidAnchor (x,y) then getAnchor (rotateV 0.1 (x,y)) else (x,y)
        anchor = getAnchor (1000,1000)
        countIntersections [] = 0
        countIntersections [_] = 0
        countIntersections (p1:p2:path) =
            case intersectSegSeg p1 p2 pt anchor of
                Nothing -> countIntersections (p2:path)
                Just _ -> 1 + countIntersections (p2:path)

liesOnLine :: Point -> Point -> Point -> Bool
liesOnLine (x1,y1) (x2,y2) (x,y) = 
    dotV (x1-x, y1-y) (y2-y, x-x2) == 0