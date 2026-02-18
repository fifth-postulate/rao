module VectorTest exposing (suite)

import Algebra.Vector as Vector exposing (Vector)
import Expect exposing (Expectation)
import Fraction
import FractionTest exposing (fraction)
import Fuzz exposing (..)
import Test exposing (..)
import Util exposing (uncurry)


suite : Test
suite =
    describe "Vector Module"
        [ describe "addition"
            [ fuzz (vector dimension) "zero is left neutral" <|
                \expected ->
                    let
                        zero =
                            Vector.zero (Vector.dimension expected)

                        actual =
                            Vector.add zero expected
                    in
                    Expect.equal actual expected
            , fuzz (vector dimension) "zero is right neutral" <|
                \expected ->
                    let
                        zero =
                            Vector.zero (Vector.dimension expected)

                        actual =
                            Vector.add expected zero
                    in
                    Expect.equal actual expected
            , fuzz2 (vector (constant 5)) (vector (constant 5)) "is commutative" <|
                \u v ->
                    let
                        uv =
                            Vector.add u v

                        vu =
                            Vector.add v u
                    in
                    Expect.equal uv vu
            ]
        , describe "scale"
            [ fuzz (vector dimension) "scale with 1 is identity" <|
                \expected ->
                    let
                        actual =
                            Vector.scale Fraction.one expected
                    in
                    Expect.equal actual expected
            , fuzz (vector dimension) "scale with 0 is zero" <|
                \v ->
                    let
                        zero =
                            Vector.zero (Vector.dimension v)

                        actual =
                            Vector.scale Fraction.zero v
                    in
                    Expect.equal actual zero
            ]
        , describe "subtract"
            [ fuzz2 (vector (constant 5)) (vector (constant 5)) "is well defined" <|
                \left right ->
                    let
                        actual =
                            Vector.subtract left right

                        thgir =
                            Vector.scale (Fraction.negate Fraction.one) right

                        expected =
                            Vector.add left thgir
                    in
                    Expect.equal actual expected
            ]
        , describe "dot"
            [ test "is well defined" <|
                \_ ->
                    let
                        u =
                            Vector.fromList [ Fraction.one, Fraction.zero, Fraction.negate Fraction.one ]

                        v =
                            Vector.fromList [ Fraction.zero, Fraction.one, Fraction.negate Fraction.one ]

                        actual =
                            Vector.dot u v

                        expected =
                            Fraction.one
                    in
                    Expect.equal actual expected
            ]
        ]


dimension : Fuzzer Int
dimension =
    intRange 1 25


vector : Fuzzer Int -> Fuzzer Vector
vector n =
    n
        |> Fuzz.andThen (\dim -> listOfLength dim fraction)
        |> Fuzz.map Vector.fromList
