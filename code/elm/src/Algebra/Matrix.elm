module Algebra.Matrix exposing (Matrix, Operation(..), element, fromList, index, rowEchelon)

import Algebra.Vector as Vector exposing (Vector)
import Array exposing (Array)
import Fraction exposing (Fraction)


type Matrix
    = Rows (Array Vector)


fromList : List Vector -> Matrix
fromList rows =
    Rows (Array.fromList rows)


index : Int -> Matrix -> Maybe Vector
index row (Rows rows) =
    Array.get row rows


element : Int -> Int -> Matrix -> Maybe Fraction
element row column matrix =
    matrix
        |> index row
        |> Maybe.andThen (Vector.index column)


rowCount : Matrix -> Int
rowCount (Rows vs) =
    Array.length vs


columnCount : Matrix -> Int
columnCount (Rows vs) =
    Array.get 0 vs
        |> Maybe.map Vector.dimension
        |> Maybe.withDefault 0


type Operation
    = Swap Int Int
    | Multiply Fraction Int
    | Linear Fraction Int Int


type SearchResult
    = NotFound
    | Found Int Int Fraction


rowEchelon : Matrix -> ( Matrix, List Operation, List Int )
rowEchelon matrix =
    let
        firstAfterRowInColumn : (Fraction -> Bool) -> Int -> Int -> Matrix -> SearchResult
        firstAfterRowInColumn predicate startRow column m =
            let
                find : Int -> SearchResult
                find row =
                    if row < rowCount m then
                        let
                            v =
                                element row column m
                                    |> Maybe.withDefault Fraction.zero
                        in
                        if predicate v then
                            Found row column v

                        else
                            find (row + 1)

                    else
                        NotFound
            in
            find startRow

        reduce : List Operation -> Int -> Int -> Matrix -> ( Matrix, List Operation )
        reduce acc skipRow currentColumn start =
            let
                reduceFrom : List Operation -> Int -> Matrix -> ( Matrix, List Operation )
                reduceFrom ac row m =
                    if row < rowCount m then
                        if row == skipRow then
                            reduceFrom acc (row + 1) m

                        else
                            let
                                c =
                                    element row currentColumn m
                                        |> Maybe.map Fraction.negate
                                        |> Maybe.withDefault Fraction.zero
                            in
                            if not (c == Fraction.zero) then
                                reduceFrom (Linear c skipRow row :: ac) (row + 1) (linear c skipRow row m)

                            else
                                reduceFrom ac (row + 1) m

                    else
                        ( m, ac )
            in
            reduceFrom acc 0 start

        go : List Operation -> List Int -> Int -> Int -> Matrix -> ( Matrix, List Operation, List Int )
        go acc pivots currentRow currentColumn m =
            if currentColumn < columnCount m then
                case firstAfterRowInColumn (Fraction.isZero >> not) currentRow currentColumn m of
                    Found r c v ->
                        if currentRow < r then
                            go (Swap r currentRow :: acc) pivots currentRow currentColumn (swap r currentRow m)

                        else if not (v == Fraction.one) then
                            let
                                v_ =
                                    v
                                        |> Fraction.invert
                                        |> Maybe.withDefault Fraction.one
                            in
                            go (Multiply v_ r :: acc) pivots currentRow currentColumn (multiplyRow v_ r m)

                        else
                            let
                                ( m_, acc_ ) =
                                    reduce acc r c m
                            in
                            go acc_ (c :: pivots) (r + 1) (c + 1) m_

                    NotFound ->
                        go acc pivots currentRow (currentColumn + 1) m

            else
                ( m, List.reverse acc, List.reverse pivots )
    in
    go [] [] 0 0 matrix


swap : Int -> Int -> Matrix -> Matrix
swap i j ((Rows vs) as matrix) =
    let
        columns =
            columnCount matrix

        atI =
            Array.get i vs
                |> Maybe.withDefault (Vector.zero columns)

        atJ =
            Array.get j vs
                |> Maybe.withDefault (Vector.zero columns)
    in
    vs
        |> Array.set i atJ
        |> Array.set j atI
        |> Rows


multiplyRow : Fraction -> Int -> Matrix -> Matrix
multiplyRow v row ((Rows rows) as matrix) =
    let
        columns =
            columnCount matrix

        scaled =
            rows
                |> Array.get row
                |> Maybe.map (Vector.scale v)
                |> Maybe.withDefault (Vector.zero columns)
    in
    rows
        |> Array.set row scaled
        |> Rows


linear : Fraction -> Int -> Int -> Matrix -> Matrix
linear v from to ((Rows rows) as matrix) =
    let
        columns =
            columnCount matrix

        zero =
            Vector.zero columns

        addend =
            rows
                |> Array.get from
                |> Maybe.map (Vector.scale v)
                |> Maybe.withDefault zero

        result =
            rows
                |> Array.get to
                |> Maybe.map (Vector.add addend)
                |> Maybe.withDefault zero
    in
    rows
        |> Array.set to result
        |> Rows
