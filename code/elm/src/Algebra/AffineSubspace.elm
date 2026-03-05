module Algebra.AffineSubspace exposing (Subspace, base, basis, create, empty, intersection, subspace)

import Algebra.Matrix as Matrix exposing (Matrix)
import Algebra.Vector as Vector exposing (Vector)
import Algebra.VectorSpace as VectorSpace exposing (VectorSpace)
import Fraction exposing (Fraction)


type Subspace
    = Subspace
        { base : Vector
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
    create b bs


create : Vector -> VectorSpace -> Subspace
create b bs =
    Subspace { base = b, basis = bs }


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


intersection : Subspace -> Subspace -> Subspace
intersection left right =
    case ( left, right ) of
        ( Subspace l, Subspace r ) ->
            if VectorSpace.isSubspace l.basis r.basis || VectorSpace.isSubspace r.basis l.basis then
                if l.base == r.base then
                    create l.base (VectorSpace.intersection l.basis r.basis)

                else
                    Empty

            else
                let
                    space =
                        VectorSpace.intersection (Debug.log "left" l.basis) (Debug.log "right" r.basis)
                            |> Debug.log "intersection"

                    b =
                        l.base
                in
                create b space

        _ ->
            Empty
