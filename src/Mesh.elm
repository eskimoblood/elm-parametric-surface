module Mesh exposing (toMesh)

import Math.Vector3 exposing (..)
import Math.Vector2 exposing (Vec2)
import Array exposing (Array)
import WebGL


type alias Point extra =
    { extra
        | position : Vec3
        , normal : Vec3
        , coord : Vec2
    }


type alias Mesh extra =
    { triangles : List ( Int, Int, Int )
    , points : Array (Point extra)
    }


generateTriangles : Int -> Int -> Mesh extra -> Mesh extra
generateTriangles height i mesh =
    if i == ((Array.length mesh.points) - 1) then
        mesh
    else if (i + 1) % height == 0 then
        generateTriangles height (i + 1) mesh
    else
        let
            updatetMesh =
                addTriangles height i mesh
        in
            generateTriangles height (i + 1) updatetMesh


addTriangles : Int -> Int -> Mesh extra -> Mesh extra
addTriangles height i mesh =
    let
        p1 =
            i

        p2 =
            i + 1

        p3 =
            if i + height >= Array.length mesh.points then
                i % height
            else
                i + height

        p4 =
            if i + height >= Array.length mesh.points then
                (i % height) + 1
            else
                i + height + 1
    in
        createTriangle ( p1, p2, p3 ) mesh
            |> createTriangle ( p3, p4, p2 )


createTriangle : ( Int, Int, Int ) -> Mesh extra -> Mesh extra
createTriangle ( p1, p2, p3 ) mesh =
    if (p1 == p3 || p2 == p3) then
        mesh
    else
        let
            v1 =
                getPoint p1 mesh.points

            v2 =
                getPoint p2 mesh.points

            v3 =
                getPoint p3 mesh.points

            updateNormal =
                calculateNormal v2.position v3.position v1.position
                    |> updateNormalInPoint
        in
            { mesh
                | triangles = ( p1, p2, p3 ) :: mesh.triangles
                , points =
                    updateNormal p1 v1 mesh.points
                        |> updateNormal p2 v2
                        |> updateNormal p3 v3
            }


calculateNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
calculateNormal v1 v2 v3 =
    let
        edge1 =
            sub v1 v3

        edge2 =
            sub v1 v2

        crossProd =
            (cross edge1 edge2)
    in
        if crossProd == (vec3 0 0 0) then
            vec3 0 0 0
        else
            (normalize crossProd)


updateNormalInPoint : Vec3 -> Int -> Point extra -> Array (Point extra) -> Array (Point extra)
updateNormalInPoint normal position point points =
    Array.set position { point | normal = add normal point.normal } points


getPoint : Int -> Array (Point extra) -> Point extra
getPoint i points =
    let
        point =
            Array.get (i % Array.length points) points
    in
        case point of
            Just p ->
                p

            Nothing ->
                Debug.crash "Cant happen"


toMesh : Int -> List { extra | position : Vec3, coord : Vec2, normal : () } -> WebGL.Mesh (Point extra)
toMesh height points =
    let
        mesh =
            generateTriangles height 0 { points = createPoint points, triangles = [] }
    in
        List.map (triangleToDrawable (Array.map normalizeNormal mesh.points)) mesh.triangles
            |> WebGL.triangles


triangleToDrawable : Array (Point extra) -> ( Int, Int, Int ) -> ( Point extra, Point extra, Point extra )
triangleToDrawable points ( p1, p2, p3 ) =
    ( getPoint p1 points
    , getPoint p2 points
    , getPoint p3 points
    )


createPoint : List { extra | position : Vec3, coord : Vec2, normal : () } -> Array (Point extra)
createPoint vecs =
    List.map (\v -> { v | normal = vec3 1 1 1 }) vecs
        |> Array.fromList


normalizeNormal : Point extra -> Point extra
normalizeNormal point =
    { point | normal = normalize point.normal }
