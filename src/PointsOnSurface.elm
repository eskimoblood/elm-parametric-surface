module PointsOnSurface exposing (Surface(..), pointsOnSurface)

import Math.Vector3 exposing (..)


type Surface
    = Sphere
    | Moebius Float Float


getSettings : Surface -> Setting
getSettings surface =
    case surface of
        Sphere ->
            Setting 0 (2 * pi) 0 pi calcSphere

        Moebius minTheta maxTheta ->
            Setting 0 (2 * pi) minTheta maxTheta calcMoebius


calcSphere : Float -> Float -> Vec3
calcSphere phi theta =
    vec3 (sin theta * cos phi) (sin theta * sin phi) (cos theta)


calcMoebius : Float -> Float -> Vec3
calcMoebius phi theta =
    vec3
        ((1 + theta / 2 * cos (phi / 2)) * cos phi)
        ((1 + theta / 2 * cos (phi / 2)) * sin phi)
        (theta / 2 * sin (phi / 2))


steps : Int -> Float -> Float -> List Float
steps steps min max =
    let
        stepSize =
            (max - min) / (toFloat steps)
    in
        List.range 0 steps |> List.map (\i -> min + stepSize * (toFloat i))


type alias Setting =
    { minPhi : Float
    , maxPhi : Float
    , minTheta : Float
    , maxTheta : Float
    , calc : Float -> Float -> Vec3
    }


pointsOnSurface : Int -> Int -> Surface -> List (List Vec3)
pointsOnSurface phiStepsSize thetaStepsSize surface =
    let
        { minPhi, maxPhi, minTheta, maxTheta, calc } =
            getSettings surface

        phiSteps =
            steps phiStepsSize minPhi maxPhi

        thetaSteps =
            steps thetaStepsSize minTheta maxTheta
    in
        List.map
            (\phi ->
                List.map
                    (\theta ->
                        calc phi theta
                    )
                    thetaSteps
            )
            phiSteps
