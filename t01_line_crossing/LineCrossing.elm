module LineCrossing where

-- Imports
import Svg exposing (Svg, line, svg, g, circle)
import Svg.Attributes as SVGA
import Svg.Events exposing (onMouseDown, onMouseUp, onMouseMove, onClick)
import Signal exposing (message)
import Window
import Mouse
import Graphics.Collage exposing (collage, toForm)
import Geometry exposing (getIntersection, Point, Line)
import Html exposing ( Html )
import Html.Attributes as HTMLA
import Array exposing ( Array, fromList, set, get, toIndexedList)
import List exposing ( drop, map, concat, filterMap, indexedMap, concat, append )

-- MODEL

-- Identifiers of the dots on each side of a line
type DotID = Dot1 | Dot2

-- Represents the current state of the application
type State 
  = Idle
  | AddingLine
  | FinishingLine Point
  | Moving Int DotID


-- AddingLine Nothing means you are adding the first point
-- AddingLine (Just p) means you added a first point p and are adding the second

type alias Model = {
  state: State,
  lines: Array Line
}

initialX : List Float
initialX = [50, 100, 150, 200, 250]

initialLines : List Line
initialLines = map (\x -> Line (Point x 100) (Point x 250)) initialX

initialModel : Model
initialModel = Model Idle (fromList initialLines)

-- UPDATE

-- Defines the types of actions that can occurr
type Action
  = NoOp
  | StartMoving Int DotID
  | Move Int Int
  | StopMoving
  | StartAdding
  | AddPoint Int Int
  | Reset

-- Update the model given an action and the current model
update : Action -> Model -> Model
update action model = case (model.state, action) of
  (_, Reset)                       -> initialModel
  (Idle, StartMoving l d)          -> { model | state <- Moving l d }
  (Idle, StartAdding)              -> { model | state <- AddingLine }
  (Moving _ _, StopMoving)         -> { model | state <- Idle}
  (Moving _ _, Move x y)           -> move (Point (toFloat x) (toFloat y)) model
  (AddingLine, AddPoint x y)       -> { model | state <- FinishingLine (Point (toFloat x) (toFloat y)) }
  (FinishingLine p, AddPoint x y)  -> { model |
    state <- Idle,
    lines <- Array.push (Line p (Point (toFloat x) (toFloat y))) model.lines 
  }
  _                                -> model

-- Transform the current moving line (from the model) by moving one of 
-- the dots (defined on the model too) to a given point
move : Point -> Model -> Model
move p model = case model.state of
 Moving lid did -> case (get lid model.lines) of
   Just l -> { model | lines <- set lid (moveLine p did l) model.lines }
   _      -> model
 _              -> model

-- Creates a new line with one given dot moved to a given point
moveLine : Point -> DotID -> Line -> Line
moveLine p did l = case did of
  Dot1 -> { l | p1 <- p }
  Dot2 -> { l | p2 <- p }

-- VIEW

-- A function to create a message to be sent on events describing the action
actionMessage : Action -> Signal.Message
actionMessage = Signal.message actions.address

-- Renders the full view of the application
view : Model -> (Int, Int) -> (Int, Int) -> Html
view model (w,h) (x,y) = Html.div []
  [ ( Html.div []
      [ Html.button [ onClick (actionMessage StartAdding) ] [Html.text "Add"]
      , Html.button [ onClick (actionMessage Reset) ] [Html.text "Reset"]
      , Html.text (toString model.state)
      ]
    )
  , ( Html.div
      [ HTMLA.style 
        [ ("position", "absolute")
        , ("top", "40px")
        , ("bottom", "0")
        , ("left", "0")
        , ("right", "0")
        , ("border-top", "1px solid #000")
        , ("cursor", case model.state of
            Moving _ _      -> "pointer"
            AddingLine      -> "crosshair"
            FinishingLine _ -> "crosshair"
            _               -> "auto"
          )
        ]
      ]
      [ scene model (w,h-45) (x,y-41) ]
    )
  ]

-- Renders the svg containing the lines, dots and interssections
scene : Model -> (Int, Int) -> (Int, Int) -> Html
scene m (w,h) (x,y) = svg
  [ SVGA.version "1.1"
  , SVGA.x "0"
  , SVGA.y "0"
  , SVGA.width (toString w)
  , SVGA.height (toString h)
  , onMouseUp (actionMessage StopMoving)
  , onMouseMove (actionMessage (Move x y))
  , onClick (actionMessage (AddPoint x y))
  ] 
  (includeAddingLine (Point (toFloat x) (toFloat y)) m.state ( concat 
    [ (renderLines m.lines)
    , (renderIntersections m.lines)
    ]
  ))

-- Includes a dashed line corresponding to the new adding line 
-- only if you are adding a line
includeAddingLine : Point -> State -> List Svg -> List Svg
includeAddingLine p1 state svgs = case state of
    FinishingLine p2 -> (dashedLine (Line p1 p2))::svgs
    _                -> svgs

-- Render all the lines with their holders and indexed events
renderLines : Array Line -> List Svg
renderLines lines =
  map renderLineWithHolders (toIndexedList lines)

-- Render a given line passing it index forward to the renderHolder function
-- so it'll know from what line and what part of the line it belongs
renderLineWithHolders : (Int, Line) -> Svg
renderLineWithHolders (lid, l) = g []
  [ line' l []
  , renderHolder lid Dot1 l.p1
  , renderHolder lid Dot2 l.p2
  ]

-- Renders a line point given the line index and the DotID and
-- set a mouseDown event that keeps it context information (Line index and DotID)
renderHolder : Int -> DotID -> Point -> Svg
renderHolder lid did p = circle' p
  [ SVGA.r "4"
  , SVGA.fill "white"
  , SVGA.style "cursor:pointer"
  , onMouseDown (actionMessage (StartMoving lid did))
  ]

-- Render all intersections for the given lines
renderIntersections : Array Line -> List Svg
renderIntersections lines = filterMap renderIntersection (pairs (Array.toList lines))

-- Given two lines, render a dot in their interssections, if this happens
renderIntersection : (Line, Line) -> Maybe Svg
renderIntersection (l1, l2) = 
  Maybe.map (\p -> circle' p
    [ SVGA.r "6"
    , SVGA.fill "red"
    ]
    ) (getIntersection l1 l2)

-- SVG abstractions to match some concepts such as rendering points and lines

-- Renders a line
line' : Line -> List Svg.Attribute -> Svg
line' l attrs = line (append attrs
  [ SVGA.x1 (toString l.p1.x)
  , SVGA.y1 (toString l.p1.y)
  , SVGA.x2 (toString l.p2.x)
  , SVGA.y2 (toString l.p2.y)
  , SVGA.style "stroke:black;stroke-width:1"
  ]) []

-- Render the dashed line
dashedLine : Line -> Svg
dashedLine l = line' l [ SVGA.strokeDasharray "5, 5" ]

-- Renders a point as a circle
circle' : Point -> List Svg.Attribute -> Svg
circle' p attrs = circle (append attrs
  [ SVGA.cx (toString p.x)
  , SVGA.cy (toString p.y)
  , SVGA.stroke "black"
  ]) []
  

-- INPUTS
 
-- Wire the current model, the window dimensions and mouse position to the scene
main : Signal Html
main =
  Signal.map3 view model Window.dimensions Mouse.position

-- Actions from user input
actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

-- model over time
model : Signal Model
model = Signal.foldp update initialModel actions.signal

-- HELPERS

-- Makes all possible pairs of the list (Order does not matter)
pairs : List a -> List (a,a)
pairs list =
  list
    |> indexedMap (\i a ->
      list
        |> drop i
        |> map (\b -> (a,b))
      )
    |> concat
