module Geometry where

-- DATA
type alias Point = {
  x: Float,
  y: Float
}

type alias Line = {
  p1: Point,
  p2: Point
}


-- FUNCTIONS
pp : Point -> Point -> Point
pp p1 p2 = Point (p1.x + p2.x) (p1.y + p2.y)

pm : Point -> Point -> Point
pm p1 p2 = Point (p1.x - p2.x) (p1.y - p2.y)

pt : Point -> Float -> Point
pt {x,y} f = Point (x*f) (y*f)

getIntersection : Line -> Line -> Maybe Point
getIntersection {p1, p2} l2 = let
    p3 = l2.p1
    p4 = l2.p2
    s1 = pm p2 p1
    s2 = pm p4 p3

    s = (-s1.y * (p1.x - p3.x) + s1.x * (p1.y - p3.y)) / (-s2.x * s1.y + s1.x * s2.y)
    t = ( s2.x * (p1.y - p3.y) - s2.y * (p1.x - p3.x)) / (-s2.x * s1.y + s1.x * s2.y)
  in
    if (s >= 0 && s <= 1 && t >= 0 && t <= 1) then Just (pp p1 (pt s1 t)) else Nothing


