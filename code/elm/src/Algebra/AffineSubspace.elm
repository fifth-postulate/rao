module Algebra.AffineSubspace exposing (Subspace, base, subspace)

import Algebra.Vector as Vector exposing (Vector)
import Fraction exposing (Fraction)


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
