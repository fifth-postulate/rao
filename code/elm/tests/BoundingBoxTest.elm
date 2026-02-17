module BoundingBoxTest exposing (suite)

import BoundingBox exposing (BoundingBox, boundingBox)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Test exposing (..)
import Util exposing (uncurry)


suite : Test
suite =
    describe "BoundingBox Module"
        [ describe "boundingBox"
            [ test "of empty list is box around (0,0)" <|
                \_ ->
                    let
                        actual =
                            boundingBox []

                        expected =
                            BoundingBox.box 0 0 0 0
                    in
                    Expect.equal actual expected
            , test "of corners of a triangle is a square" <|
                \_ ->
                    let
                        actual =
                            boundingBox [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ) ]

                        expected =
                            BoundingBox.box 0 0 1 1
                    in
                    Expect.equal actual expected
            ]
        , describe "expand"
            [ test "adds a border around a bounding box" <|
                \_ ->
                    let
                        actual =
                            [ ( 0, 0 ), ( 2, 0 ), ( 0, 1 ) ]
                                |> boundingBox
                                |> BoundingBox.expand 2

                        expected =
                            BoundingBox.box -1 -1 4 3
                    in
                    Expect.equal actual expected
            ]
        , describe "squareUp"
            [ test "finds the smallest square centered on the bounding box" <|
                \_ ->
                    let
                        actual =
                            [ ( 0, 0 ), ( 3, 0 ), ( 0, 1 ) ]
                                |> boundingBox
                                |> BoundingBox.squareUp

                        expected =
                            BoundingBox.box 0 -1 3 3
                    in
                    Expect.equal actual expected
            ]
        ]
