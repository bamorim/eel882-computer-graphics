module Cubes where

import Color exposing (Color, toRgb, green)
import Graphics.Element exposing (Element)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, makeRotate, makePerspective, makeLookAt)
import WebGL exposing (..)

main : Element
main = webgl (800,800) (scene defaultModel)

-- UTIL

colorToVec : Color -> Vec3
colorToVec color =
  let c = toRgb color in
  vec3
      (toFloat c.red / 255)
      (toFloat c.green / 255)
      (toFloat c.blue / 255)

-- MODEL
type alias Vertex =
  { color: Vec3
  , position: Vec3
  }
 
type alias Cube =
  { color: Vec3
  , position: Vec3
  , rotation: Mat4
  }

type alias Model =
  { camera: Vec3
  , cubes: List Cube
  }

defaultModel = Model (vec3 0 0 5) [ cube green ]

cube : Color -> Cube
cube color = Cube
  (colorToVec color)
  (vec3 0 0 0)
  (makeRotate 0 (vec3 1 0 0))

-- VIEW

makeDrawable : Cube -> Drawable Vertex
makeDrawable cube =
  let
    rft = vec3  1  1  1   -- right, front, top
    lft = vec3 -1  1  1   -- left,  front, top
    lbt = vec3 -1 -1  1
    rbt = vec3  1 -1  1
    rbb = vec3  1 -1 -1
    rfb = vec3  1  1 -1
    lfb = vec3 -1  1 -1
    lbb = vec3 -1 -1 -1
  in Triangle 
    (List.concat 
      [ face cube.color rft rfb rbb rbt   -- right
      , face cube.color rft rfb lfb lft   -- front
      , face cube.color rft lft lbt rbt   -- top
      , face cube.color rfb lfb lbb rbb   -- bottom
      , face cube.color lft lfb lbb lbt   -- left
      , face cube.color rbt rbb lbb lbt   -- back
      ]
    )

face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Vertex, Vertex, Vertex)
face color a b c d =
  let
    vertex position =
        Vertex color position
  in
    [ (vertex a, vertex b, vertex c)
    , (vertex c, vertex d, vertex a)
    ]

uniforms : Cube -> Vec3 -> { rotation:Mat4, perspective:Mat4, camera:Mat4, shade:Float }
uniforms cube camera =
  { rotation = cube.rotation
  , perspective = makePerspective 45 1 0.01 100
  , camera = makeLookAt camera (vec3 0 0 0) (vec3 0 1 0)
  , shade = 0.8
  }

scene : Model -> List Renderable
scene model = List.map
  (renderCube model.camera)
  model.cubes

renderCube : Vec3 -> Cube -> Renderable
renderCube camera cube = render vertexShader fragmentShader (makeDrawable cube) (uniforms cube camera)

-- SHADERS

vertexShader : Shader { attr | position:Vec3, color:Vec3 }
                      { unif | rotation:Mat4, perspective:Mat4, camera:Mat4 }
                      { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * rotation * vec4(position, 1.0);
    vcolor = color;
}

|]


fragmentShader : Shader {} { u | shade:Float } { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
