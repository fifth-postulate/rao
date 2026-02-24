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
        ]
