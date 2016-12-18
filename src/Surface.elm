module Surface exposing (sphere)

import Math.Vector3 exposing (..)
import Mesh exposing (triangleMesh)
import WebGL exposing (..)
import PointsOnSurface exposing (Surface(..), pointsOnSurface)

type alias Vertex =
    { color : Vec3
    , position : Vec3
    }
    


sphere : Drawable Vertex
sphere =
    (pointsOnSurface 40 20 (Moebius -5 5))
        |> triangleMesh
        |> List.map
            (\( v1, v2, v3 ) ->
                ( Vertex v1 v1
                , Vertex v2 v2
                , Vertex v3 v3
                )
            )
        |> Triangle
