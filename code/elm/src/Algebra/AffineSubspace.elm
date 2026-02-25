module Algebra.AffineSubspace exposing (Subspace, base, basis, empty, subspace)

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
    | Empty


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


empty : Subspace
empty =
    Empty


base : Subspace -> Maybe Vector
base space =
    case space of
        Subspace s ->
            Just s.base

        Empty ->
            Nothing


basis : Subspace -> Maybe VectorSpace
basis space =
    case space of
        Subspace s ->
            Just s.basis

        Empty ->
            Nothing
