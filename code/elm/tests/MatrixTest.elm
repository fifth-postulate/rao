module MatrixTest exposing (suite)

import Algebra.Matrix as Matrix exposing (Matrix)
import Algebra.Vector as Vector exposing (Vector)
import Expect exposing (Expectation)
import Fraction exposing (Fraction)
import Fuzz exposing (..)
import Test exposing (..)
import Util exposing (uncurry)


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
        ]
