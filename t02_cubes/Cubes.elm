module Objects where

import Window
import Graphics.Element exposing (Element)
import Display
import Array exposing (set, get, fromList)
import Model exposing (Model, Object, xrotate, yrotate)
import Time
import Keyboard
import Set exposing (Set, member)

main : Signal Element
main = Signal.map2 Display.view Window.dimensions model

model : Signal Model
model = Signal.foldp update Model.defaultModel events

events : Signal (Object -> Object)
events = Signal.map alterObject sampleKeys

sampleKeys : Signal (Set Keyboard.KeyCode)
sampleKeys = Signal.sampleOn (Time.fps 40) Keyboard.keysDown

alterObject : (Set Keyboard.KeyCode) -> Object -> Object
alterObject keys =
  case ((member 37 keys), (member 38 keys), (member 39 keys), (member 40 keys)) of
    (True, False, False, False) -> xrotate -0.1
    (False, True, False, False) -> yrotate -0.1
    (False, False, True, False) -> xrotate 0.1
    (False, False, False, True) -> yrotate 0.1
    _                           -> (\c -> c)


update : (Object -> Object) -> Model -> Model
update fn model =
  let
    originalObject = get 0 model.cubes
  in case originalObject of
    Just object -> Model (fromList [(fn object)])
    _           -> model
