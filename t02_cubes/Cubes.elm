module Cubes where

import Window
import Graphics.Element exposing (Element)
import Display

main : Signal Element
main = Signal.map Display.view Window.dimensions
