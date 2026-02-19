module VectorSpaceTest exposing (suite)

import Algebra.Vector as Vector
import Algebra.VectorSpace as VectorSpace exposing (VectorSpace, span)
import Expect exposing (Expectation)
import Fraction
import Fuzz exposing (..)
import Test exposing (..)
import Util exposing (uncurry)


suite : Test
suite =
    describe "VectorSpace Module"
        [ describe "creation"
            [ test "fromList creates a VectorSpace" <|
                \_ ->
                    let
                        v =
                            [ 1, 2, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        actual =
                            v
                                |> List.singleton
                                |> VectorSpace.span

                        expected =
                            VectorSpace.empty
                                |> VectorSpace.add v
                    in
                    Expect.equal actual expected
            ]
        , describe "contains"
            [ test "vector which is a multiple of a single vector is contained" <|
                \_ ->
                    let
                        b =
                            [ 1, 2, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        v =
                            Vector.scale (Fraction.fromInt 2) b

                        space =
                            VectorSpace.span [ b ]
                    in
                    toBeTrue (VectorSpace.contains v space)
            ]
        ]


toBeTrue : Bool -> Expectation
toBeTrue b =
    if b then
        Expect.pass

    else
        Expect.fail "expected to be True"
