module Mesh exposing (triangleMesh)

import Math.Vector3 exposing (..)
import List.Extra exposing (find)


squareToTriangles : a -> a -> a -> a -> List ( a, a, a )
squareToTriangles topLeft botLeft topRight botRight =
    [ ( topLeft, botLeft, topRight )
    , ( topRight, botRight, botLeft )
    ]


triangles : List ( a, a ) -> List ( a, a, a )
triangles list =
    case list of
        ( tl, tr ) :: ((( bl, br ) :: _) as rest) ->
            List.append
                (squareToTriangles tl bl tr br)
                (triangles rest)

        _ ->
            []


triangleMesh : List (List a) -> List ( a, a, a )
triangleMesh list =
    case list of
        left :: ((right :: _) as rest) ->
            List.append
                (triangles <| List.map2 (,) left right)
                (triangleMesh rest)

        _ ->
            []


type alias Triangle =
    { points : ( Vertex, Vertex, Vertex )
    , id : String
    , normal : Vec3
    }


type alias Vertex =
    { position : Vec3
    , id : String
    , normal : Vec3
    , triangles : List String
    }


calcFaceNormal : Triangle -> Triangle
calcFaceNormal triangle =
    let
        ( v1, v2, v3 ) =
            triangle.points

        edge1 =
            sub v2.position v1.position

        edge2 =
            sub v3.position v1.position
    in
        { triangle | normal = (cross edge1 edge2) |> normalize }


pointIsInTriangle : ( Vertex, Vertex, Vertex ) -> Vertex -> Bool
pointIsInTriangle ( v1, v2, v3 ) v =
    v.id == v1.id || v.id == v2.id || v.id == v3.id


findTriangles : List Triangle -> Vertex -> List Triangle
findTriangles triangles vertex =
    List.filter (\{ points } -> pointIsInTriangle points vertex) triangles


calcVertextNormal : List Triangle -> Vertex -> Vertex
calcVertextNormal triangles vertex =
    let
        t =
            findTriangles triangles vertex

        normal =
            List.foldl (\t r -> add r t.normal) (vec3 0 0 0) t
                |> normalize
    in
        { vertex | normal = normal }


updateVertices : List Vertex -> Triangle -> { position : ( Vertex, Vertex, Vertex ) }
updateVertices vertices { points } =
    let
        ( v1, v2, v3 ) =
            points

        v1_r =
            find (\v -> v.id == v1.id) vertices

        v2_r =
            find (\v -> v.id == v2.id) vertices

        v3_r =
            find (\v -> v.id == v3.id) vertices
    in
        case ( v1_r, v2_r, v2_r ) of
            ( Just a, Just b, Just c ) ->
                { position = ( a, b, c ) }

            _ ->
                Debug.crash "SHOULD NOT HAPPEN"


calcNormals : List Vertex -> List Triangle -> List { position : ( Vertex, Vertex, Vertex ) }
calcNormals vertices triangles =
    let
        triangleWithNormals =
            List.map calcFaceNormal triangles

        vertiecesWithNormals =
            List.map (calcVertextNormal triangles) vertices
    in
        List.map (updateVertices vertiecesWithNormals) triangles
