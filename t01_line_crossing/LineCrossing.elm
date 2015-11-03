module LineCrossing where

-- Imports
import Svg exposing (Svg, line, svg, g, circle)
import Svg.Attributes as SVGA
import Svg.Events exposing (onMouseDown, onMouseUp, onMouseMove)
import Signal exposing (message)
import Window
import Mouse
import Geometry exposing (getIntersection, Point, Line)
import Html exposing ( Html )
import Array exposing ( Array, fromList, set, get, toIndexedList)
import List exposing ( append )

-- MODEL

-- Identifiers to find the exact dot
type DotID = Dot1 | Dot2

-- Represents the currently moving dot
type Moving = NotMoving | Moving Int DotID

type alias Model = {
  lines: Array Line,
  moving: Moving
}

initialLine1 : Line
initialLine1 = Line (Point 100 100) (Point 100 250)

initialLine2 : Line
initialLine2 = Line (Point 250 100) (Point 250 250)

initialX : List Float
initialX = [50, 100, 150, 200, 250]

initialLines : List Line
initialLines = List.map (\x -> Line (Point x 100) (Point x 250)) initialX

initialModel : Model
initialModel = Model (fromList initialLines) NotMoving

-- UPDATE

type Action
  = NoOp
  | StartMoving Int DotID
  | Move Int Int
  | StopMoving

-- Update the model given an action
update : Action -> Model -> Model
update action model =
  case action of
    StopMoving      -> { model | moving <- NotMoving}
    StartMoving l d -> { model | moving <- Moving l d }
    Move x y        -> move (Point (toFloat x) (toFloat y)) model
    _               -> model
 
-- Move currently moving dot to the given point

moveLine : Point -> DotID -> Line -> Line
moveLine p did l = case did of
  Dot1 -> { l | p1 <- p }
  Dot2 -> { l | p2 <- p }


move : Point -> Model -> Model
move p model = case model.moving of
 Moving lid did -> case (get lid model.lines) of
   Just l -> { model | lines <- set lid (moveLine p did l) model.lines }
   _      -> model
 _              -> model
 

-- VIEW
scene : Model -> (Int, Int) -> (Int, Int) -> Html
scene m (w,h) (x,y) = svg
  [ SVGA.version "1.1",
    SVGA.x "0",
    SVGA.y "0",
    SVGA.width (toString w),
    SVGA.height (toString h),
    onMouseUp (Signal.message actions.address StopMoving),
    onMouseMove (Signal.message actions.address (Move x y))
  ] 
  (append (showLines m.lines) (showIntersections m.lines))

showLines : Array Line -> List Svg
showLines lines =
  List.map showLineWithHolders (toIndexedList lines)

showLineWithHolders : (Int, Line) -> Svg
showLineWithHolders (lid, l) = g []
  [ showLine l.p1 l.p2,
    showPoint lid Dot1 l.p1,
    showPoint lid Dot2 l.p2
  ]

showPoint : Int -> DotID -> Point -> Svg
showPoint lid did {x, y} = circle 
  [ SVGA.cx (toString x), 
    SVGA.cy (toString y),
    SVGA.r "4", SVGA.stroke "black",
    SVGA.fill "white",
    onMouseDown (Signal.message actions.address (StartMoving lid did))
  ] []

showLine : Point -> Point -> Svg
showLine p1 p2 = line
  [ SVGA.x1 (toString p1.x),
    SVGA.y1 (toString p1.y),
    SVGA.x2 (toString p2.x),
    SVGA.y2 (toString p2.y),
    SVGA.style "stroke:black;stroke-width:1"
  ] []
  

showIntersections : Array Line -> List Svg
showIntersections lines = List.map showIntersection (combine (Array.toList lines))

combine : List a -> List (a,a)
combine list = List.concatMap (\a -> (List.map (\b -> (a,b)) list)) list

showIntersection : (Line, Line) -> Svg
showIntersection (l1, l2) = case (getIntersection l1 l2) of
  Just {x, y} -> circle
    [ SVGA.cx (toString x), 
      SVGA.cy (toString y),
      SVGA.r "6", SVGA.stroke "black",
      SVGA.fill "red"
    ] []
  _           -> g [] []

-- INPUTS
 
-- Wire the current model, the window dimensions and mouse position to the scene
main : Signal Html
main =
  Signal.map3 scene model Window.dimensions Mouse.position

-- Actions from user input
actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

-- model over time
model : Signal Model
model = Signal.foldp update initialModel actions.signal
