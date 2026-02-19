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


rowEchelon : Matrix -> ( Matrix, List Operation )
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

        go : List Operation -> Int -> Int -> Matrix -> ( Matrix, List Operation )
        go acc currentRow currentColumn m =
            if currentColumn < columnCount m then
                case firstAfterRowInColumn (Fraction.isZero >> not) currentRow currentColumn m of
                    Found r _ _ ->
                        if currentRow < r then
                            go (Swap r currentRow :: acc) currentRow currentColumn (swap r currentRow m)
                            -- ( m, acc )

                        else
                            ( m, acc )

                    NotFound ->
                        go acc currentRow (currentColumn + 1) m

            else
                ( m, acc )
    in
    go [] 0 0 matrix


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
