module Type exposing (..)

{-|
# Type
@docs Type
-}


{-|
Surface types then can be created with the package

* `Sphere`
* `Moebius innerRadius outerRadius`
* `Roman`
* `SuperShape a b m1 n1_1 n1_2 n1_3 m2 n2_1 n2_2 n2_3`
-}
type Type
    = Sphere
    | Moebius Float Float
    | Roman
    | SuperShape Float Float Float Float Float Float Float Float Float Float
