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
            [ test "span creates a VectorSpace" <|
                \_ ->
                    let
                        v =
                            [ 1, 2, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        actual =
                            vectorSpace [ [ 1, 2, 3 ] ]

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
            , test "vector which is a sum of two vectors is contained" <|
                \_ ->
                    let
                        b1 =
                            [ 1, 2, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        b2 =
                            [ 3, 1, 3 ]
                                |> List.map Fraction.fromInt
                                |> Vector.fromList

                        v =
                            Vector.add b1 b2

                        space =
                            VectorSpace.span [ b1, b2 ]
                    in
                    toBeTrue (VectorSpace.contains v space)
            ]
        , describe "intersection"
            [ test "works as expected" <|
                \_ ->
                    let
                        u =
                            vectorSpace [ [ 1, 0, 0 ], [ 0, 1, 0 ] ]

                        v =
                            vectorSpace [ [ 1, 1, -1 ], [ 1, 1, 1 ] ]

                        actual =
                            VectorSpace.intersection u v

                        expected =
                            vectorSpace [ [ 1, 1, 0 ] ]
                    in
                    toBeTrue (VectorSpace.equals actual expected)
            ]
        ]


vectorSpace : List (List Int) -> VectorSpace
vectorSpace vectors =
    vectors
        |> List.map (List.map Fraction.fromInt >> Vector.fromList)
        |> VectorSpace.span


toBeTrue : Bool -> Expectation
toBeTrue b =
    if b then
        Expect.pass

    else
        Expect.fail "expected to be True"
