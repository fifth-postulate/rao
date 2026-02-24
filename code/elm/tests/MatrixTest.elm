module MatrixTest exposing (suite)

import Algebra.Matrix as Matrix exposing (Matrix, Operation(..))
import Algebra.Vector as Vector exposing (Vector)
import Expect
import Fraction
import Fuzz exposing (..)
import Test exposing (..)
import VectorSpaceTest as Setup


toVector : List Int -> Vector
toVector coefficients =
    coefficients
        |> List.map Fraction.fromInt
        |> Vector.fromList


matrix : List (List Int) -> Matrix
matrix vectors =
    vectors
        |> List.map toVector
        |> Matrix.fromList


suite : Test
suite =
    describe "Matrix Module"
        [ describe "element"
            [ test "1 2 correctly determined" <|
                \_ ->
                    let
                        actual =
                            [ [ 1, 2, 3, 4, 5 ]
                            , [ 6, 7, 8, 9, 0 ]
                            ]
                                |> matrix
                                |> Matrix.element 1 2

                        expected =
                            Fraction.fromInt 8
                                |> Just
                    in
                    Expect.equal actual expected
            , test "2 2 correctly determined" <|
                \_ ->
                    let
                        actual =
                            [ [ 1, 2, 3, 4, 5 ]
                            , [ 6, 7, 8, 9, 0 ]
                            ]
                                |> matrix
                                |> Matrix.element 2 2

                        expected =
                            Nothing
                    in
                    Expect.equal actual expected
            , test "-1 2 correctly determined" <|
                \_ ->
                    let
                        actual =
                            [ [ 1, 2, 3, 4, 5 ]
                            , [ 6, 7, 8, 9, 0 ]
                            ]
                                |> matrix
                                |> Matrix.element 2 2

                        expected =
                            Nothing
                    in
                    Expect.equal actual expected
            , test "0 -1 correctly determined" <|
                \_ ->
                    let
                        actual =
                            [ [ 1, 2, 3, 4, 5 ]
                            , [ 6, 7, 8, 9, 0 ]
                            ]
                                |> matrix
                                |> Matrix.element 0 -1

                        expected =
                            Nothing
                    in
                    Expect.equal actual expected
            , test "0 5 correctly determined" <|
                \_ ->
                    let
                        actual =
                            [ [ 1, 2, 3, 4, 5 ]
                            , [ 6, 7, 8, 9, 0 ]
                            ]
                                |> matrix
                                |> Matrix.element 0 5

                        expected =
                            Nothing
                    in
                    Expect.equal actual expected
            ]
        , describe "row echelon"
            [ test "a matrix in row echelon form isn't transformed" <|
                \_ ->
                    let
                        expected =
                            [ [ 1, 2, 0 ]
                            , [ 0, 0, 1 ]
                            ]
                                |> matrix
                    in
                    Matrix.rowEchelon expected
                        |> Expect.all
                            [ \( actual, _, _ ) -> Expect.equal actual expected
                            , \( _, operations, _ ) -> Expect.equal operations []
                            , \( _, _, pivots ) -> Expect.equal pivots [ 0, 2 ]
                            ]
            , test "a matrix that needs to be swapped will" <|
                \_ ->
                    let
                        start =
                            [ [ 0, 0, 1 ]
                            , [ 1, 2, 0 ]
                            ]
                                |> matrix

                        expected =
                            [ [ 1, 2, 0 ]
                            , [ 0, 0, 1 ]
                            ]
                                |> matrix
                    in
                    Matrix.rowEchelon start
                        |> Expect.all
                            [ \( actual, _, _ ) -> Expect.equal actual expected
                            , \( _, operations, _ ) -> Expect.equalLists operations [ Swap 1 0 ]
                            , \( _, _, pivots ) -> Expect.equal pivots [ 0, 2 ]
                            ]
            , test "a matrix that needs to be multiplied will" <|
                \_ ->
                    let
                        start =
                            [ [ 0, 0, 1 ]
                            , [ 2, 4, 0 ]
                            ]
                                |> matrix

                        expected =
                            [ [ 1, 2, 0 ]
                            , [ 0, 0, 1 ]
                            ]
                                |> matrix

                        halve =
                            Fraction.fromInt 2
                                |> Fraction.invert
                                |> Maybe.withDefault Fraction.one
                    in
                    Matrix.rowEchelon start
                        |> Expect.all
                            [ \( actual, _, _ ) -> Expect.equal actual expected
                            , \( _, operations, _ ) -> Expect.equalLists operations [ Swap 1 0, Multiply halve 0 ]
                            , \( _, _, pivots ) -> Expect.equal pivots [ 0, 2 ]
                            ]
            , test "a matrix that needs to be linearize will" <|
                \_ ->
                    let
                        start =
                            [ [ 0, 0, 1 ]
                            , [ 2, 4, 0 ]
                            , [ 0, 0, 2 ]
                            ]
                                |> matrix

                        expected =
                            [ [ 1, 2, 0 ]
                            , [ 0, 0, 1 ]
                            , [ 0, 0, 0 ]
                            ]
                                |> matrix

                        halve =
                            Fraction.fromInt 2
                                |> Fraction.invert
                                |> Maybe.withDefault Fraction.one
                    in
                    Matrix.rowEchelon start
                        |> Expect.all
                            [ \( actual, _, _ ) -> Expect.equal actual expected
                            , \( _, operations, _ ) ->
                                Expect.equalLists operations
                                    [ Swap 1 0
                                    , Multiply halve 0
                                    , Linear (Fraction.fromInt 2 |> Fraction.negate) 1 2
                                    ]
                            , \( _, _, pivots ) -> Expect.equal pivots [ 0, 2 ]
                            ]
            , test "A computational Introduction to Number Theory and Algebra (p325)" <|
                \_ ->
                    let
                        start =
                            [ [ 0, 1, -2, 0, 0, 3 ]
                            , [ 0, 0, 0, 1, 0, 2 ]
                            , [ 0, 0, 0, 0, 1, -4 ]
                            , [ 0, 0, 0, 0, 0, 0 ]
                            ]
                                |> matrix

                        expected =
                            [ [ 0, 1, -2, 0, 0, 3 ]
                            , [ 0, 0, 0, 1, 0, 2 ]
                            , [ 0, 0, 0, 0, 1, -4 ]
                            , [ 0, 0, 0, 0, 0, 0 ]
                            ]
                                |> matrix
                    in
                    Matrix.rowEchelon start
                        |> Expect.all
                            [ \( actual, _, _ ) -> Expect.equal actual expected
                            , \( _, _, pivots ) -> Expect.equal pivots [ 1, 3, 4 ]
                            ]
            ]
        , describe "identity"
            [ test "correct matrix" <|
                \_ ->
                    let
                        actual =
                            Matrix.identityMatrix 3

                        expected =
                            [ [ 1, 0, 0 ]
                            , [ 0, 1, 0 ]
                            , [ 0, 0, 1 ]
                            ]
                                |> matrix
                    in
                    Expect.equal actual expected
            ]
        , describe "kernel"
            [ test "correct VectorSpace for matrix in rowEchelon" <|
                \_ ->
                    let
                        start =
                            [ [ 1, 2, 0, 3, 0 ]
                            , [ 0, 0, 1, 2, 0 ]
                            , [ 0, 0, 0, 0, 1 ]
                            , [ 0, 0, 0, 0, 0 ]
                            ]
                                |> matrix

                        actual =
                            Matrix.kernel start

                        expected =
                            [ [ 0, 0, 0, 1 ]
                            ]
                                |> Setup.vectorSpace
                    in
                    Expect.equal actual expected
            , test "correct VectorSpace for matrix a swap from rowEchelon" <|
                \_ ->
                    let
                        start =
                            [ [ 1, 2, 0, 3, 0 ]
                            , [ 0, 0, 1, 2, 0 ]
                            , [ 0, 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0, 1 ]
                            ]
                                |> matrix

                        actual =
                            Matrix.kernel start

                        expected =
                            [ [ 0, 0, 1, 0 ]
                            ]
                                |> Setup.vectorSpace
                    in
                    Expect.equal actual expected
            , test "correct VectorSpace for matrix linear from rowEchelon" <|
                \_ ->
                    let
                        start =
                            [ [ 1, 2, 0, 3, 0 ]
                            , [ 0, 0, 1, 2, 0 ]
                            , [ 0, 0, 0, 0, 1 ]
                            , [ 0, 0, 0, 0, 2 ]
                            ]
                                |> matrix

                        actual =
                            Matrix.kernel start

                        expected =
                            [ [ 0, 0, -2, 1 ] ]
                                |> Setup.vectorSpace
                    in
                    Expect.equal actual expected
            ]
        , describe "transpose"
            [ test "correctly transforms a matrix" <|
                \_ ->
                    let
                        actual =
                            [ [ 1, 2, 3, 4 ]
                            , [ 5, 6, 7, 8 ]
                            ]
                                |> matrix
                                |> Matrix.transpose

                        expected =
                            [ [ 1, 5 ]
                            , [ 2, 6 ]
                            , [ 3, 7 ]
                            , [ 4, 8 ]
                            ]
                                |> matrix
                    in
                    Expect.equal expected actual
            ]
        ]
