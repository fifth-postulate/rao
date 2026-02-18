module AffineSubspace exposing (Subspace, base, subspace)

import Fraction exposing (Fraction)
import Vector exposing (Vector)


type Subspace
    = Subspace
        { normal : Vector
        , q : Fraction
        , base : Vector
        , basis : List Vector
        }


subspace : Vector -> Fraction -> Subspace
subspace n q =
    let
        t =
            Vector.dot n n

        s =
            Fraction.divide q t
                |> Maybe.withDefault Fraction.one

        b =
            Vector.scale s n
    in
    Subspace
        { normal = n
        , q = q
        , base = b
        , basis = []
        }


base : Subspace -> Vector
base (Subspace space) =
    space.base
