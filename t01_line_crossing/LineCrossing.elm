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

-- MODEL

-- Identifiers to find the exact line and dot
type LineID = Line1 | Line2
type DotID = Dot1 | Dot2

-- Represents the currently moving dot
type Moving = NotMoving | Moving LineID DotID

type alias Model = {
  line1: Line,
  line2: Line,
  moving: Moving
}

initialLine1 : Line
initialLine1 = Line (Point 100 100) (Point 100 250)

initialLine2 : Line
initialLine2 = Line (Point 250 100) (Point 250 250)

initialModel : Model
initialModel = Model initialLine1 initialLine2 NotMoving

-- UPDATE

type Action
  = NoOp
  | StartMoving LineID DotID
  | Move Int Int
  | StopMoving

-- Move currently moving dot to the given point
move : Point -> Model -> Model
move p model =
  case model.moving of
    Moving Line1 Dot1 -> let
      line = model.line1
      newLine = { line | p1 <- p }
    in
      { model | line1 <- newLine }
    Moving Line2 Dot1 -> let
      line = model.line2
      newLine = { line | p1 <- p }
    in
      { model | line2 <- newLine }
    Moving Line1 Dot2 -> let
      line = model.line1
      newLine = { line | p2 <- p }
    in
      { model | line1 <- newLine }
    Moving Line2 Dot2 -> let
      line = model.line2
      newLine = { line | p2 <- p }
    in
      { model | line2 <- newLine }
    _ -> model
      

-- Update the model given an action
update : Action -> Model -> Model
update action model =
  case action of
    StopMoving      -> { model | moving <- NotMoving}
    StartMoving l d -> { model | moving <- Moving l d }
    Move x y        -> move (Point (toFloat x) (toFloat y)) model
    _               -> model
  

-- VIEW
scene : (Model) -> (Int, Int) -> (Int, Int) -> Html
scene m (w,h) (x,y) = svg
  [ SVGA.version "1.1",
    SVGA.x "0",
    SVGA.y "0",
    SVGA.width (toString w),
    SVGA.height (toString h),
    onMouseUp (Signal.message actions.address StopMoving),
    onMouseMove (Signal.message actions.address (Move x y))
  ] 
  [ showLineWithHolders Line1 m.line1,
    showLineWithHolders Line2 m.line2,
    showIntersection m.line1 m.line2
  ]


showLineWithHolders : LineID -> Line -> Svg
showLineWithHolders lid l = g []
  [ showLine l.p1 l.p2,
    showPoint lid Dot1 l.p1,
    showPoint lid Dot2 l.p2
  ]

showIntersection : Line -> Line -> Svg
showIntersection l1 l2 = case (getIntersection l1 l2) of
  Just {x, y} -> circle
    [ SVGA.cx (toString x), 
      SVGA.cy (toString y),
      SVGA.r "4", SVGA.stroke "black",
      SVGA.fill "white"
    ] []
  _           -> g [] []

showPoint : LineID -> DotID -> Point -> Svg
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
    SVGA.style "stroke:rgb(255,0,0);stroke-width:1"
  ] []
  

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
