module Algebra.AffineSubspace exposing (Subspace, base, basis, subspace)

import Algebra.Matrix as Matrix exposing (Matrix)
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

        bs =
            [ n ]
                |> Matrix.fromList
                |> Matrix.transpose
                |> Matrix.kernel
    in
    Subspace
        { normal = n
        , q = q
        , base = b
        , basis = bs
        }


base : Subspace -> Vector
base (Subspace space) =
    space.base


basis : Subspace -> VectorSpace
basis (Subspace space) =
    space.basis
