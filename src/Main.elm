module Main exposing (..)

import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html
import Html.Attributes exposing (width, height)
import Surface.Surface exposing (surface)
import Surface.Type exposing (..)
import Mouse exposing (..)


type alias Model =
    { x : Int
    , y : Int
    }


initialModel : Model
initialModel =
    { x = 0
    , y = 0
    }


type Msg
    = Position Int Int


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        Position x y ->
            ( { model | x = x, y = y }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves (\{ x, y } -> Position x y)


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = scene >> WebGL.toHtml  [ width 1000, height 1000 ]
        , subscriptions = subscriptions
        , update = update
        }



-- VIEW


s =
    (surface (SuperShape 0.6 1.312 8 20 8 3 30 5 2 2) 100 100)



-- (surface (Sphere) 20 20)


scene : Model -> List Renderable
scene { x, y } =
    [ render vertexShader fragmentShader s (uniforms (toFloat x / 100) (toFloat y / 100)) ]


uniforms : Float -> Float -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniforms x y =
    { rotation = mul (makeRotate (3 * x) (vec3 0 1 0)) (makeRotate (2 * y) (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 300
    , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.6
    }


-- SHADERS


vertexShader : Shader { attr | position : Vec3, normal : Vec3, coord : Vec2, color: Vec3 } { unif | rotation : Mat4, perspective : Mat4, camera : Mat4 } { vertPos : Vec3, normalInterp : Vec3, c : Vec2, cl: Vec3 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 normal;
attribute vec2 coord;
attribute vec3 color;

uniform mat4 camera, perspective, rotation;

varying vec3 normalInterp;
varying vec3 vertPos;
varying vec2 c;
varying vec3 cl;


void main(){
    gl_Position = perspective * camera * rotation * vec4(position, 1.0);
    vertPos = position;
    normalInterp = normal;
    c = coord;
    cl = color;
}

|]


fragmentShader : Shader {} { u | shade : Float } { vertPos : Vec3, normalInterp : Vec3, c : Vec2, cl: Vec3 }
fragmentShader =
    [glsl|

precision mediump float;

varying vec3 normalInterp;
varying vec3 vertPos;
varying vec2 c;
varying vec3 cl;


const int mode = 2;

const vec3 lightPos = vec3(1.0,1.0,1.0);
const vec3 ambientColor = vec3(1, 0.0, 0.0);
const vec3 diffuseColor = vec3(0.2, 0.2, 0.2);
const vec3 specColor = vec3(0.3, 0.3, 0.2);

float modI(float a,float b) {
    float m=a-floor((a+0.5)/b)*b;
    return floor(m+0.5);
}

void main() {

  vec3 normal = normalize(normalInterp);
  vec3 lightDir = normalize(lightPos - vertPos);

  float lambertian = max(dot(lightDir,normal), 0.0);
  float specular = 0.0;

  if(lambertian > 0.0) {

    vec3 viewDir = normalize(-vertPos);

    // this is blinn phong
    vec3 halfDir = normalize(lightDir + viewDir);
    float specAngle = max(dot(halfDir, normal), 0.0);
    specular = pow(specAngle, 16.0);

    // this is phong (for comparison)
    if(mode == 2) {
      vec3 reflectDir = reflect(-lightDir, normal);
      specAngle = max(dot(reflectDir, viewDir), 0.0);
      // note that the exponent is different here
      specular = pow(specAngle, 4.0);
    }
  }


  gl_FragColor = vec4( cl +
                      lambertian * diffuseColor +
                      specular * specColor, 1.0);
}
|]
