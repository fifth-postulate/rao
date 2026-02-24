module Algebra.AffineSubspace exposing (Subspace, base, subspace)

import Algebra.Vector as Vector exposing (Vector)
import Algebra.VectorSpace as VectorSpace exposing (VectorSpace)
import Fraction exposing (Fraction)


type Subspace
    = Subspace
        { normal : Vector
        , q : Fraction
        , base : Vector
        , basis : VectorSpace
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
        , basis = VectorSpace.empty
        }


base : Subspace -> Vector
base (Subspace space) =
    space.base
