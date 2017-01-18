module Surface.Surface exposing (surface)

import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import WebGL exposing (..)
import Surface.Mesh exposing (toMesh)
import Surface.Type exposing (..)
import Surface.PointsOnSurface exposing (pointsOnSurface)
import Color exposing (Color, rgb, toRgb)
import Noise exposing(..)
import Random exposing (initialSeed)
import Math.Vector2 exposing (..)
import Color.Gradient exposing (..)
import Color.Interpolate exposing(..)
import Array exposing (fromList)
type alias Vertex =
    { normal : Vec3
    , position : Vec3
    , coord : Vec2
    , color: Vec3
    }

(perm, newSeed) = permutationTable (initialSeed 90) -- generate the permutation table

g : Gradient
g =
  [ (0, rgb 215 25 0)
  , (0.40, rgb 255 255 255)
  , (0.70, rgb 0 100 255)
  , (1, rgb 215 25 0)
  ]

palette : Array.Array Color
palette = Array.fromList (gradientFromStops RGB g 500)

surface : Type -> Int -> Int -> Drawable Vertex
surface surfaceType width height =
    pointsOnSurface (width) (height) surfaceType
        |> addColorToList
        |> toMesh (height + 1)

addColorToList : List { position : Vec3, coord : Vec2 } -> List { position : Vec3, coord : Vec2 , color: Vec3} 
addColorToList list =
    List.map addColor list

addColor : { position : Vec3, coord : Vec2 } -> { position : Vec3, coord : Vec2 , color: Vec3} 
addColor {position, coord} = 
    let
        f =  noise2d perm  (Math.Vector2.getX coord / 20) (Math.Vector2.getY coord / 200)
        noiseValue = 
            if f < 0 then
               (noise3d perm (Math.Vector3.getX position / 10) (Math.Vector3.getY position / 10)  (Math.Vector3.getZ position / 1) ) 

            else 
                noise2d perm  (Math.Vector2.getX coord / 0.001) (Math.Vector2.getX coord / 100)
        {red, green, blue, alpha} =  getColor (floor (noiseValue * toFloat (Array.length palette))) |> toRgb 
    in
        {position= position, coord = coord, color = vec3 ((toFloat red) / 255.0)  ((toFloat green) / 255.0) ((toFloat blue) / 255.0)}


getColor : Int -> Color
getColor i =
    let
      color = Array.get (i%(Array.length palette)) palette
    in
        case color of
          Just c ->
                c            
          Nothing ->
            Debug.crash "will never happen"
       