module Surface.Surface exposing (surface)

import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import WebGL exposing (..)
import Surface.Mesh exposing (toMesh)
import Surface.Type exposing (..)
import Surface.PointsOnSurface exposing (pointsOnSurface)


type alias Vertex =
    { normal : Vec3
    , position : Vec3
    , coord : Vec2
    }


surface : Type -> Int -> Int -> Drawable Vertex
surface surfaceType width height =
    pointsOnSurface (width) (height) surfaceType
        |> toMesh (height + 1)
