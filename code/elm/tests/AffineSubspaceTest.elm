module AffineSubspaceTest exposing (suite)

import Algebra.AffineSubspace as Subspace exposing (subspace)
import Algebra.Vector as Vector
import Algebra.VectorSpace as VectorSpace
import Expect
import Fraction
import Fuzz exposing (..)
import Test exposing (..)
import TestUtils exposing (toBeTrue)
import VectorSpaceTest as Setup


suite : Test
suite =
    describe "AffineSubspace Module"
        [ describe "subspace"
            [ test "base of a subspace can be determined" <|
                \_ ->
                    let
                        normal =
                            [ 1, 2, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        q =
                            Fraction.fromInt 2

                        space =
                            subspace normal q

                        actual =
                            Subspace.base space

                        t =
                            Fraction.divide q (Fraction.fromInt 14)
                                |> Maybe.withDefault Fraction.one

                        expected =
                            Vector.scale t normal
                                |> Just
                    in
                    Expect.equal actual expected
            , test "basis of a subspace can be determined" <|
                \_ ->
                    let
                        normal =
                            [ 1, 2, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        q =
                            Fraction.fromInt 2

                        space =
                            subspace normal q

                        actual =
                            Subspace.basis space
                                |> Maybe.withDefault VectorSpace.empty

                        t =
                            Fraction.divide q (Fraction.fromInt 14)
                                |> Maybe.withDefault Fraction.one

                        expected =
                            [ [ 2, -1, 0 ]
                            , [ 3, 0, -1 ]
                            ]
                                |> Setup.vectorSpace
                    in
                    toBeTrue (VectorSpace.equals actual expected)
            ]
        , describe "intersection"
            [ test "with yourself result in self" <|
                \_ ->
                    let
                        normal =
                            [ 1, 2, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        q =
                            Fraction.fromInt 2

                        expected =
                            subspace normal q

                        actual =
                            Subspace.intersection expected expected
                    in
                    Expect.equal actual expected
            , test "with disjoint spaces results in Empty" <|
                \_ ->
                    let
                        normal =
                            [ 1, 2, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        q =
                            Fraction.fromInt 2

                        q_ =
                            Fraction.fromInt 4

                        expected =
                            Subspace.empty

                        actual =
                            Subspace.intersection
                                (subspace normal q)
                                (subspace normal q_)
                    in
                    Expect.equal actual expected
            , test "with contained spaces results in smalles space" <|
                \_ ->
                    let
                        normal =
                            [ 1, 0, 0 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        null =
                            [ 0, 1, 0 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        q =
                            Fraction.fromInt 1

                        large =
                            subspace normal q

                        small =
                            Subspace.create normal (VectorSpace.span [ null ])

                        expected =
                            small

                        actual =
                            Subspace.intersection
                                large
                                small
                    in
                    Expect.equal actual expected
            ]
        ]
