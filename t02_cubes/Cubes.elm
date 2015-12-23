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
model = Signal.constant Model.defaultModel
