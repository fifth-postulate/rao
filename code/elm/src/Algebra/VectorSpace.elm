module Algebra.VectorSpace exposing (VectorSpace, add, contains, empty, span)

import Algebra.Vector as Vector exposing (Vector)
import Fraction exposing (Fraction)
import Util exposing (uncurry, zip)


type VectorSpace
    = VectorSpace
        { basis : List Vector
        }


span : List Vector -> VectorSpace
span basis =
    List.foldl add empty basis


empty : VectorSpace
empty =
    VectorSpace { basis = [] }


contains : Vector -> VectorSpace -> Bool
contains v space =
    let
        w =
            projection v space
    in
    v == w


projection : Vector -> VectorSpace -> Vector
projection v (VectorSpace { basis }) =
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


project : Vector -> Vector -> Fraction
project v b =
    Fraction.divide (Vector.dot b v) (Vector.dot b b)
        |> Maybe.withDefault Fraction.zero


add : Vector -> VectorSpace -> VectorSpace
add v ((VectorSpace vs) as space) =
    if contains v space then
        space

    else
        VectorSpace
            { basis = List.append vs.basis [ v ]
            }
