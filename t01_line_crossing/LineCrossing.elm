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
import List exposing ( append )

-- MODEL

-- Identifiers to find the exact dot
type DotID = Dot1 | Dot2

-- Represents the currently moving dot
type State = Idle | AddingPoint | Moving Int DotID

type alias Model = {
  state: State,
  lines: Array Line,
  firstPointAdding: Maybe Point
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
initialModel = Model Idle (fromList initialLines) Nothing

-- UPDATE

type Action
  = NoOp
  | StartMoving Int DotID
  | Move Int Int
  | StopMoving
  | StartAdding
  | AddPoint Int Int

-- Update the model given an action
update : Action -> Model -> Model
update action model = case (model.state, action) of
  (Idle, StartMoving l d)     -> { model | state <- Moving l d }
  (Idle, StartAdding)         -> { model | state <- AddingPoint }
  (Moving _ _, StopMoving)    -> { model | state <- Idle}
  (Moving _ _, Move x y)      -> move (Point (toFloat x) (toFloat y)) model
  (AddingPoint, AddPoint x y) -> case model.firstPointAdding of
    Nothing -> { model | firstPointAdding <- Just (Point (toFloat x) (toFloat y)) }
    Just p  -> { model |
      firstPointAdding <- Nothing,
      state <- Idle,
      lines <- Array.push (Line p (Point (toFloat x) (toFloat y))) model.lines 
    }
  _               -> model

-- Move currently moving dot to the given point
moveLine : Point -> DotID -> Line -> Line
moveLine p did l = case did of
  Dot1 -> { l | p1 <- p }
  Dot2 -> { l | p2 <- p }


move : Point -> Model -> Model
move p model = case model.state of
 Moving lid did -> case (get lid model.lines) of
   Just l -> { model | lines <- set lid (moveLine p did l) model.lines }
   _      -> model
 _              -> model
 

-- VIEW
actionMessage = Signal.message actions.address

view : Model -> (Int, Int) -> (Int, Int) -> Html
view model (w,h) (x,y) = Html.div []
  [ ( Html.div []
      [ Html.button [ onClick (actionMessage StartAdding) ] [Html.text "Add"]
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
            Moving _ _  -> "pointer"
            AddingPoint -> "crosshair"
            _           -> "auto"
          )
        ]
      ]
      [ scene model (w,h-45) (x,y-41) ]
    )
  ]

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
  ( List.concat 
    [ (showLines m.lines)
    , (showIntersections m.lines)
    ]
  )

showLines : Array Line -> List Svg
showLines lines =
  List.map showLineWithHolders (toIndexedList lines)

showLineWithHolders : (Int, Line) -> Svg
showLineWithHolders (lid, l) = g []
  [ showLine l.p1 l.p2
  , showPoint lid Dot1 l.p1
  , showPoint lid Dot2 l.p2
  ]

showPoint : Int -> DotID -> Point -> Svg
showPoint lid did {x, y} = circle 
  [ SVGA.cx (toString x)
  , SVGA.cy (toString y)
  , SVGA.r "4"
  , SVGA.stroke "black"
  , SVGA.fill "white"
  , SVGA.style "cursor:pointer"
  , onMouseDown (actionMessage (StartMoving lid did))
  ] []

showLine : Point -> Point -> Svg
showLine p1 p2 = line
  [ SVGA.x1 (toString p1.x)
  , SVGA.y1 (toString p1.y)
  , SVGA.x2 (toString p2.x)
  , SVGA.y2 (toString p2.y)
  , SVGA.style "stroke:black;stroke-width:1"
  ] []
  

showIntersections : Array Line -> List Svg
showIntersections lines = List.map showIntersection (crossProduct (Array.toList lines) (Array.toList lines))

crossProduct : List a -> List b -> List (a,b)
crossProduct list_a list_b = 
  List.concatMap (\a -> (List.map (\b -> (a,b)) list_b)) list_a

showIntersection : (Line, Line) -> Svg
showIntersection (l1, l2) = case (getIntersection l1 l2) of
  Just {x, y} -> circle
    [ SVGA.cx (toString x)
    , SVGA.cy (toString y)
    , SVGA.r "6"
    , SVGA.stroke "black"
    , SVGA.fill "red"
    ] []
  _           -> g [] []

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
