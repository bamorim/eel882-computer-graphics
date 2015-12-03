module Display where

import Graphics.Element exposing (Element)
import WebGL exposing (webgl, Renderable, render, Drawable(Triangle), Shader)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, makePerspective, makeTranslate, makeLookAt)
import Model exposing (Model, Object)

type alias Vertex =
  { color: Vec3
  , position: Vec3
  , normal: Vec3
  }
 

view : (Int,Int) -> Element
view dim = webgl dim (scene dim Model.defaultModel)

scene : (Int,Int) -> Model -> List Renderable
scene dim model = List.map
  (renderCube dim)
  model.cubes

renderCube : (Int,Int) -> Object -> Renderable
renderCube dims cube = render vertexShader fragmentShader (cubeToTriangles cube) (uniforms dims cube)

cubeToTriangles : Object -> Drawable Vertex
cubeToTriangles cube =
  let
    right  = (vec3  1  0  0)
    left   = (vec3 -1  0  0)
    front  = (vec3  0  1  0)
    back   = (vec3  0 -1  0)
    top    = (vec3  0  0  1)
    bottom = (vec3  0  0 -1)

    rft = (vec3  1  1  1)   -- right, front, top
    lft = (vec3 -1  1  1)   -- left,  front, top
    lbt = (vec3 -1 -1  1)
    rbt = (vec3  1 -1  1)
    rbb = (vec3  1 -1 -1)
    rfb = (vec3  1  1 -1)
    lfb = (vec3 -1  1 -1)
    lbb = (vec3 -1 -1 -1)
  in Triangle 
    (List.concat 
      [ face cube.color rft rfb rbb rbt right
      , face cube.color rft rfb lfb lft front
      , face cube.color rft lft lbt rbt top
      , face cube.color rfb lfb lbb rbb bottom
      , face cube.color lft lfb lbb lbt left
      , face cube.color rbt rbb lbb lbt back
      ]
    )

face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Vertex, Vertex, Vertex)
face color a b c d normal =
  let
    vertex position =
        Vertex color position normal
  in
    [ (vertex a, vertex b, vertex c)
    , (vertex c, vertex d, vertex a)
    ]

perspective : (Int,Int) -> Mat4
perspective (w,h) = makePerspective 45 ((toFloat w)/(toFloat h)) 0.01 100

uniforms : (Int,Int) -> Object -> Uniforms
uniforms dims cube =
  { rotation = cube.rotation
  , translation = makeTranslate cube.position
  , perspective = perspective dims
  , shade = 0.8
  , camera = makeLookAt (vec3 0 0 30) (vec3 0 0 0) (vec3 0 1 0)
  }

-- SHADERS

type alias VSAttrs a =
  { a | position: Vec3, color: Vec3, normal: Vec3}
type alias VSUniforms a =
  { a | rotation:Mat4, translation: Mat4, perspective: Mat4, camera: Mat4 }

type alias FSUniforms a = { a | shade: Float }
type alias Uniforms = VSUniforms (FSUniforms {})

type alias Varyings = { vcolor: Vec3 }

vertexShader : Shader (VSAttrs attr) (VSUniforms unif) Varyings
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
uniform mat4 translation;
varying vec3 vcolor;
void main () {
  mat4 model = translation * rotation;
  mat4 view = camera;
  mat4 projection = perspective;
  gl_Position = projection * view * model * vec4(position, 1.0);
  vec3 projected_normal = normalize(mat3(view * model) * normal);
  vcolor = projected_normal.z*color;
}

|]


fragmentShader : Shader {} (FSUniforms unif) Varyings
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;
void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]
