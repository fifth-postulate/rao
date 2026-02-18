module AffineSubspaceTest exposing (suite)

import Algebra.AffineSubspace as Subspace exposing (Subspace, subspace)
import Algebra.Vector as Vector
import Expect exposing (Expectation)
import Fraction
import Fuzz exposing (..)
import Test exposing (..)
import Util exposing (uncurry)


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
            ]
        ]
