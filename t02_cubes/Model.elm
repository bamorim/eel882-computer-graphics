module Model where
import Math.Vector3 exposing (Vec3, vec3, add)
import Math.Vector4 exposing (Vec4, vec4)
import Math.Matrix4 exposing (Mat4, makeRotate)
import Array exposing (Array, fromList)
import Maybe exposing (andThen)
-- UTIL

import Color exposing (Color, toRgb, green, yellow)
colorToVec : Color -> Vec4
colorToVec color =
  let c = toRgb color in
  vec4
      (toFloat c.red / 255)
      (toFloat c.green / 255)
      (toFloat c.blue / 255)
      1.0

-- MODEL
type alias Object =
  { color: Vec4
  , position: Vec3
  , rotation: Mat4
  }

type alias Model =
  {
    cubes: Array Object,
    selected: Maybe Int
  }

getSelected : Model -> Maybe Object
getSelected model =
  model.selected `andThen` (\i -> Array.get i model.cubes) 

defaultModel : Model
defaultModel =
  Model
    ( fromList
      [ sampleObject1
      , sampleObject2
      ]
    )
    (Just 1)

sampleObject1 : Object
sampleObject1 = 
  cube green
    |> rotate (vec3 1 0 0) 1
    |> rotate (vec3 0 1 0) 1
    |> rotate (vec3 1 0 0) 1

sampleObject2 : Object
sampleObject2 = 
  cube yellow
    |> translate (vec3 5 5 0)

-- create a centered cube
cube : Color -> Object
cube color = Object
  (colorToVec color)
  (vec3 0 0 0)
  (makeRotate 0 (vec3 1 0 0))

-- ACTION

translate : Vec3 -> Object -> Object
translate ds cube = { cube | position <- (add ds cube.position) }

rotate : Vec3 -> Float -> Object -> Object
rotate axis alpha cube = { cube | rotation <- Math.Matrix4.rotate alpha axis cube.rotation }

xrotate : Float -> Object -> Object
xrotate = rotate (vec3 1 0 0)

yrotate : Float -> Object -> Object
yrotate = rotate (vec3 0 1 0)
