module Algebra.VectorSpace exposing (VectorSpace, add, contains, empty, equals, intersection, span)

import Algebra.Vector as Vector exposing (Vector)
import Fraction exposing (Fraction)
import Util exposing (swap, uncurry, zip)


type VectorSpace
    = VectorSpace
        { basis : List Vector
        }
    | Origin


span : List Vector -> VectorSpace
span basis =
    List.foldl add empty basis


empty : VectorSpace
empty =
    Origin


contains : Vector -> VectorSpace -> Bool
contains v space =
    let
        w =
            projection v space
    in
    v == w


projection : Vector -> VectorSpace -> Vector
projection v space =
    case space of
        VectorSpace { basis } ->
            let
                coefficients =
                    basis
                        |> List.map (project v)

                components =
                    zip coefficients basis
                        |> List.map (uncurry Vector.scale)
            in
            case components of
                c :: cs ->
                    List.foldl Vector.add c cs

                [] ->
                    v

        Origin ->
            Vector.zero (Vector.dimension v)


project : Vector -> Vector -> Fraction
project v b =
    Fraction.divide (Vector.dot b v) (Vector.dot b b)
        |> Maybe.withDefault Fraction.zero


add : Vector -> VectorSpace -> VectorSpace
add v space =
    case space of
        VectorSpace vs ->
            if contains v space then
                space

            else
                let
                    p =
                        projection v space

                    n =
                        Vector.subtract v p
                in
                VectorSpace
                    { basis = List.append vs.basis [ n ]
                    }

        Origin ->
            VectorSpace { basis = [ v ] }


intersection : VectorSpace -> VectorSpace -> VectorSpace
intersection u v =
    case ( u, v ) of
        ( VectorSpace left, VectorSpace _ ) ->
            left.basis
                |> List.map (swap projection v)
                |> span

        _ ->
            Origin


equals : VectorSpace -> VectorSpace -> Bool
equals u v =
    isSubSpace u v && isSubSpace v u


isSubSpace : VectorSpace -> VectorSpace -> Bool
isSubSpace u v =
    case u of
        VectorSpace { basis } ->
            List.all (swap contains v) basis

        Origin ->
            True
