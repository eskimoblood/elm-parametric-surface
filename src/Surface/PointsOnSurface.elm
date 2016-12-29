module Surface.PointsOnSurface exposing (pointsOnSurface)

import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import List.Extra exposing (lift2)
import Surface.Formulars as F
import Surface.Type exposing (..)


getSettings : Type -> F.Setting
getSettings surface =
    case surface of
        Sphere ->
            F.sphere

        Moebius minTheta maxTheta ->
            F.moebius minTheta maxTheta

        Roman ->
            F.roman

        SuperShape a b m1 n1_1 n1_2 n1_3 m2 n2_1 n2_2 n2_3 ->
            F.superShape a b m1 n1_1 n1_2 n1_3 m2 n2_1 n2_2 n2_3


steps : Int -> Float -> Float -> List ( Float, Float )
steps steps min max =
    let
        stepSize =
            (max - min) / (toFloat steps)
    in
        List.range 0 steps |> List.indexedMap (\i a -> ( min + stepSize * (toFloat i), (toFloat i) ))


pointsOnSurface : Int -> Int -> Type -> List { position : Vec3, coord : Vec2 }
pointsOnSurface phiStepsSize thetaStepsSize surface =
    let
        { minPhi, maxPhi, minTheta, maxTheta, calc } =
            getSettings surface

        phiSteps =
            steps phiStepsSize minPhi maxPhi |> List.take (phiStepsSize)

        thetaSteps =
            steps thetaStepsSize minTheta maxTheta
    in
        lift2 (\( phi, i ) ( theta, j ) -> { position = calc phi theta, coord = vec2 i j }) phiSteps thetaSteps
