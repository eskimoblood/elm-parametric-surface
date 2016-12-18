module Main exposing (..)

import Color exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html
import AnimationFrame
import Html.Attributes exposing (width, height)
import Time exposing (Time)
import Surface exposing (sphere)


main : Program Never Time Time
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = scene >> WebGL.toHtml [ width 400, height 400 ]
        , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
        , update = (\dt theta -> ( theta + dt / 5000, Cmd.none ))
        }



-- MESHES - create a cube in which each vertex has a position and color


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }





-- VIEW


scene : Float -> List Renderable
scene angle =
    [ render vertexShader fragmentShader sphere (uniforms angle) ]


uniforms : Float -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniforms t =
    { rotation = mul (makeRotate (3 * t) (vec3 0 1 0)) (makeRotate (2 * t) (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.2
    }



-- SHADERS


vertexShader : Shader { attr | position : Vec3, color : Vec3 } { unif | rotation : Mat4, perspective : Mat4, camera : Mat4 } { vcolor : Vec3 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * rotation * vec4(position, 1.0);
    vcolor = position;
}

|]


fragmentShader : Shader {} { u | shade : Float } { vcolor : Vec3 }
fragmentShader =
    [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]



