module Surface exposing (surface, surfaceExtended, Vertex, ExtentFunc)

{-|
# Surface
@docs surface
@docs surfaceExtended
@docs Vertex
@docs ExtentFunc
-}

import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import WebGL exposing (..)
import Mesh exposing (toMesh)
import Type exposing (..)
import PointsOnSurface exposing (pointsOnSurface)
import Math.Vector2 exposing (..)


{-|
The vertex record that is used as the type of the resulting `Mesh`.

* `position`: position of the vertex
* `normal`: the normal interpolated by the normal of all the triangles that contains the vertex
* `coord`: value  between 0 and resolution width / height
* `extra`: any additional field you can add by `ExtentFunc`
-}
type alias Vertex extra =
    { extra
        | normal : Vec3
        , position : Vec3
        , coord : Vec2
    }


{-|
The function that can be used to extend the basic vertex, with additional attributes. The function needs to return the basic
attributes `position`, `coord` and `normal` plus the fields defined in extra. As it is not possible to add fields to an exisiting
record you have to copy all the fields plus the extra into a new record.

```
addColor : ExtentFn {color : Vec3}
addColor { position, coord, normal } =
        { normal = normal, position = position, coord = coord, color = position}
```
-}
type alias ExtentFunc extra =
    { position : Vec3, coord : Vec2, normal : () } -> { extra | position : Vec3, coord : Vec2, normal : () }


{-|
Creates a mesh from a given `Type`, `width` and `height`


-}
surface : Type -> Int -> Int -> Mesh (Vertex {})
surface surfaceType width height =
    pointsOnSurface (width) (height) surfaceType
        |> toMesh (height + 1)


{-|
Creates a mesh from a given `Type`, `width`, `height`and a function to extend the vertex.
The extend function can be used to add additional attributes, like colour, to your vertecies.

-}
surfaceExtended : Type -> Int -> Int -> ExtentFunc extra -> Mesh (Vertex extra)
surfaceExtended surfaceType width height extent =
    pointsOnSurface (width) (height) surfaceType
        |> List.map extent
        |> toMesh (height + 1)
