module Display where

import Graphics.Element exposing (Element)
import WebGL exposing (webgl, Renderable, renderWithConfig, Drawable(Triangle), Shader, FunctionCall(Enable,BlendFunc), Capability(Blend), BlendOperation(SrcAlpha, OneMinusSrcAlpha))
import Math.Vector3 exposing (Vec3, vec3, scale)
import Math.Vector4 exposing (Vec4, vec4)
import Math.Matrix4 exposing (Mat4, makePerspective, makeTranslate, makeLookAt)
import Model exposing (Model, Object,getSelected)
import Array exposing (Array,toList)

type alias Vertex =
  { color: Vec4
  , position: Vec3
  , normal: Vec3
  }
 
-- Setup our render function with our configurations
render =
  renderWithConfig
    [ Enable Blend
    , BlendFunc (SrcAlpha,OneMinusSrcAlpha)
    ]

view : (Int,Int) -> Model -> Element
view dim model = webgl dim (scene dim model)

scene : (Int,Int) -> Model -> List Renderable
scene dim model =
  List.concat
    [ (renderSelection dim model)
    , (renderCubes dim model.cubes)
    ]

renderSelection : (Int,Int) -> Model -> List Renderable
renderSelection dim model =
  case getSelected model of
    Just object ->
      [ renderSelected dim object ]

    Nothing ->
      []

renderSelected : (Int,Int) -> Object -> Renderable
renderSelected dim selectedCube =
  let
      cube = { selectedCube | color <- (vec4 0.2 0.2 0.2 0.5) }
      shape = (drawCube 1.1 cube)
      --shape = (drawSphere 2 cube.color)
  in 
      render
        vertexShader
        fragmentShader
        shape
        (uniforms dim cube)

renderCubes : (Int,Int) -> Array Object -> List Renderable
renderCubes dim objects =
  List.map
    (renderCube dim)
    (Array.toList objects)

renderCube : (Int,Int) -> Object -> Renderable
renderCube dims cube = render vertexShader fragmentShader (drawCube 1.0 cube) (uniforms dims cube)

-- Create shapes
type alias Square =
  (Vec3, Vec3, Vec3, Vec3)
drawCube : Float -> Object -> Drawable Vertex
drawCube size cube =
  let
    right  = scale size (vec3  1  0  0)
    left   = scale size (vec3 -1  0  0)
    front  = scale size (vec3  0  1  0)
    back   = scale size (vec3  0 -1  0)
    top    = scale size (vec3  0  0  1)
    bottom = scale size (vec3  0  0 -1)

    rft = scale size (vec3  1  1  1)   -- right, front, top
    lft = scale size (vec3 -1  1  1)   -- left,  front, top
    lbt = scale size (vec3 -1 -1  1)
    rbt = scale size (vec3  1 -1  1)
    rbb = scale size (vec3  1 -1 -1)
    rfb = scale size (vec3  1  1 -1)
    lfb = scale size (vec3 -1  1 -1)
    lbb = scale size (vec3 -1 -1 -1)
  in Triangle 
    (List.concat 
      [ face cube.color (rft,rfb,rbb,rbt) right
      , face cube.color (rft,rfb,lfb,lft) front
      , face cube.color (rft,lft,lbt,rbt) top
      , face cube.color (rfb,lfb,lbb,rbb) bottom
      , face cube.color (lft,lfb,lbb,lbt) left
      , face cube.color (rbt,rbb,lbb,lbt) back
      ]
    )

face : Vec4 -> Square -> Vec3 -> List (Vertex, Vertex, Vertex)
face color (a,b,c,d) normal =
  let
    vertex position =
        Vertex color position normal
  in
    [ (vertex a, vertex b, vertex c)
    , (vertex c, vertex d, vertex a)
    ]

type alias ParametricFunction =
  (Float,Float) -> (Float,Float,Float)

type alias Range = (Float,Float)

-- The parametric function for the sphere
sphere : Float -> ParametricFunction
sphere r (t,p) =
  ( r*sin(t)*cos(p)
  , r*sin(t)*sin(p)
  , r*cos(t)
  )

-- Get the shpere drawable
drawSphere : Float -> Vec4 -> Drawable Vertex
drawSphere r color =
  drawParametric
    color
    (sphere r)
    (sphere r)
    (0,pi)
    (0,2*pi)
    20
    40

range : Int -> Int -> List Int
range from to = if from < to then from :: (range (from+1) to) else []

combine : List a -> List b -> List (a,b)
combine xs ys =
  List.concatMap (\x -> List.map (\y -> (x,y)) ys) xs

drawParametric : Vec4 -> ParametricFunction -> ParametricFunction -> Range -> Range -> Int -> Int -> Drawable Vertex
drawParametric color fn normalFn (x1,x2) (y1,y2) xslices yslices =
  let
      tripleToVec = (\(a,b,c) -> vec3 a b c)
      fn' = fn >> tripleToVec
      normalFn' = normalFn >> tripleToVec

      dx = (x2-x1)/(toFloat xslices)
      dy = (y2-y1)/(toFloat yslices)

      xs = (range 0 xslices)
        |> List.map toFloat
        |> List.map (\i -> x1 + i*dx)
      ys = (range 0 yslices)
        |> List.map toFloat
        |> List.map (\i -> y1 + i*dy)

      startingPoints = combine xs ys

      pointToFace (x,y) = face
        color
        ( fn' (x,y)
        , fn' (x+dx,y)
        , fn' (x+dx,y+dy)
        , fn' (x,y+dy)
        )
        ( normalFn' (x+dx/2,y+dy/2) )

      faces = startingPoints
        |> List.map pointToFace

  in
      Triangle (List.concat faces)

-- Rendering Math

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
  { a | position: Vec3, color: Vec4, normal: Vec3}
type alias VSUniforms a =
  { a | rotation:Mat4, translation: Mat4, perspective: Mat4, camera: Mat4 }

type alias FSUniforms a = { a | shade: Float }
type alias Uniforms = VSUniforms (FSUniforms {})

type alias Varyings = { vcolor: Vec4 }

vertexShader : Shader (VSAttrs attr) (VSUniforms unif) Varyings
vertexShader = [glsl|

attribute vec3 position;
attribute vec4 color;
attribute vec3 normal;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
uniform mat4 translation;
varying vec4 vcolor;
void main () {
  mat4 model = translation * rotation;
  mat4 view = camera;
  mat4 projection = perspective;
  gl_Position = projection * view * model * vec4(position, 1.0);
  vec3 projected_normal = normalize(mat3(view * model) * normal);
  vcolor = vec4(projected_normal.z*vec3(color),color.w);
}

|]


fragmentShader : Shader {} (FSUniforms unif) Varyings
fragmentShader = [glsl|

precision mediump float;
varying vec4 vcolor;
void main () {
    gl_FragColor = vcolor;
}

|]
