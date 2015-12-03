module Model where
import Math.Vector3 exposing (Vec3, vec3, add)
import Math.Matrix4 exposing (Mat4, makeRotate)
-- UTIL

import Color exposing (Color, toRgb, green)
colorToVec : Color -> Vec3
colorToVec color =
  let c = toRgb color in
  vec3
      (toFloat c.red / 255)
      (toFloat c.green / 255)
      (toFloat c.blue / 255)

-- MODEL
type alias Object =
  { color: Vec3
  , position: Vec3
  , rotation: Mat4
  }

type alias Model =
  {
    cubes: List Object
  }

defaultModel : Model
defaultModel = Model
  [ defaultObject
  ]

defaultObject : Object
defaultObject = 
  cube green
    |> translate (vec3 1 1 1)
    |> rotate (vec3 1 0 0) 1
    |> rotate (vec3 0 1 0) 1
    |> rotate (vec3 1 0 0) 1

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

