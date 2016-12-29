module Surface.Formulars exposing (Setting, sphere, moebius, roman, superShape)

import Math.Vector3 exposing (Vec3, vec3)


type alias Setting =
    { minPhi : Float
    , maxPhi : Float
    , minTheta : Float
    , maxTheta : Float
    , calc : Float -> Float -> Vec3
    }


twoPi : Float
twoPi =
    2 * pi


halfPi : Float
halfPi =
    pi / 2


sphere : Setting
sphere =
    { minPhi = 0
    , maxPhi = twoPi
    , minTheta = 0
    , maxTheta = pi
    , calc = calcSphere
    }


calcSphere : Float -> Float -> Vec3
calcSphere phi theta =
    vec3 (sin theta * cos phi) (sin theta * sin phi) (cos theta)


moebius : Float -> Float -> Setting
moebius minTheta maxTheta =
    { minPhi = 0
    , maxPhi = twoPi
    , minTheta = minTheta
    , maxTheta = maxTheta
    , calc = calcMoebius
    }


calcMoebius : Float -> Float -> Vec3
calcMoebius phi theta =
    vec3
        ((1 + theta / 2 * cos (phi / 2)) * cos phi)
        ((1 + theta / 2 * cos (phi / 2)) * sin phi)
        (theta / 2 * sin (phi / 2))


roman : Setting
roman =
    { minPhi = 0
    , maxPhi = twoPi
    , minTheta = -halfPi
    , maxTheta = halfPi
    , calc = calcRoman
    }


calcRoman : Float -> Float -> Vec3
calcRoman phi theta =
    vec3
        (cos phi * sin phi * sin theta)
        (cos phi * sin phi * cos theta)
        ((cos phi) ^ 2 * cos theta * sin theta)


superShape : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Setting
superShape a b m1 n1_1 n1_2 n1_3 m2 n2_1 n2_2 n2_3 =
    { minPhi = -pi
    , maxPhi = pi
    , minTheta = -halfPi
    , maxTheta = halfPi
    , calc = calcSuperShape a b m1 n1_1 n1_2 n1_3 m2 n2_1 n2_2 n2_3
    }


calcSuperShape : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Vec3
calcSuperShape a b m1 n1_1 n1_2 n1_3 m2 n2_1 n2_2 n2_3 phi theta =
    let
        r1 =
            calcR a b m1 n1_1 n1_2 n1_3 phi

        r2 =
            calcR a b m2 n2_1 n2_2 n2_3 theta
    in
        vec3
            (r1 * cos phi * r2 * cos theta)
            (r1 * sin phi * r2 * cos theta)
            (r2 * sin theta)


calcR : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
calcR a b m n1 n2 n3 angle =
    (abs (abs (1 / a * cos (m * angle / 4)) ^ n2 + abs (1 / b * sin (m * angle / 4)) ^ n3)) ^ (-1 / n1)
