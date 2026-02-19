module Algebra.Matrix exposing (Matrix, element, fromList, index)

import Algebra.Vector as Vector exposing (Vector)
import Fraction exposing (Fraction)


type Matrix
    = Rows (List Vector)


fromList : List Vector -> Matrix
fromList rows =
    Rows rows


index : Int -> Matrix -> Maybe Vector
index row (Rows rows) =
    if row < 0 then
        Nothing

    else
        rows
            |> List.drop row
            |> List.head


element : Int -> Int -> Matrix -> Maybe Fraction
element row column matrix =
    matrix
        |> index row
        |> Maybe.andThen (Vector.index column)
