module PointTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Polygon.Point as Point exposing (Point)
import Polygon.Point.Cartesian as Cartesian exposing (Cartesian)
import Polygon.Point.Polar as Polar exposing (Polar)
import Test exposing (..)


suite : Test
suite =
    describe "Point Module"
        [ fuzz cartesian "fromCartesian>>toCartesian is identity" <|
            \expected ->
                let
                    actual =
                        expected
                            |> Point.fromCartesian
                            |> Point.toCartesian
                in
                Expect.equal actual expected
        , fuzz polar "fromPolar>>toPolar is identity" <|
            \expected ->
                let
                    actual =
                        expected
                            |> Point.fromPolar
                            |> Point.toPolar
                in
                Expect.equal actual expected
        , fuzz pointCartesian "toCartesian>>fromCartesian is identity" <|
            \expected ->
                let
                    actual =
                        expected
                            |> Point.toCartesian
                            |> Point.fromCartesian
                in
                Expect.equal actual expected
        , fuzz pointPolar "toPolar>>fromPolar is identity" <|
            \expected ->
                let
                    actual =
                        expected
                            |> Point.toPolar
                            |> Point.fromPolar
                in
                Expect.equal actual expected
        , fuzz pointCartesian "scale with 1 is identity" <|
            \expected ->
                let
                    actual =
                        expected
                            |> Point.scale 1
                in
                Expect.equal actual expected
        , fuzz pointPolar "rotate with 0 is identity" <|
            \expected ->
                let
                    actual =
                        expected
                            |> Point.rotate 0
                in
                Expect.equal actual expected
        , describe "Cartesian"
            [ fuzz (pair niceFloat niceFloat) "from>>to is identity" <|
                \expected ->
                    let
                        actual =
                            expected
                                |> Cartesian.from
                                |> Cartesian.to
                    in
                    Expect.equal actual expected
            , fuzz cartesian "to>>from is identity" <|
                \expected ->
                    let
                        actual =
                            expected
                                |> Cartesian.to
                                |> Cartesian.from
                    in
                    Expect.equal actual expected
            , fuzz cartesian "translate with O is identity" <|
                \expected ->
                    let
                        actual =
                            expected
                                |> Cartesian.translate ( 0, 0 )
                    in
                    Expect.equal actual expected
            , fuzz cartesian "scale with 1 is identity" <|
                \expected ->
                    let
                        actual =
                            expected
                                |> Cartesian.scale 1
                    in
                    Expect.equal actual expected
            ]
        , describe "Polar"
            [ fuzz (pair niceFloat niceFloat) "from>>to is identity" <|
                \expected ->
                    let
                        actual =
                            expected
                                |> Polar.from
                                |> Polar.to
                    in
                    Expect.equal actual expected
            , fuzz polar "to>>from is identity" <|
                \expected ->
                    let
                        actual =
                            expected
                                |> Polar.to
                                |> Polar.from
                    in
                    Expect.equal actual expected
            , fuzz polar "rotate with O is identity" <|
                \expected ->
                    let
                        actual =
                            expected
                                |> Polar.rotate 0
                    in
                    Expect.equal actual expected
            , fuzz polar "scale with 1 is identity" <|
                \expected ->
                    let
                        actual =
                            expected
                                |> Polar.scale 1
                    in
                    Expect.equal actual expected
            ]
        ]


pointCartesian : Fuzzer Point
pointCartesian =
    cartesian
        |> Fuzz.map Point.fromCartesian


pointPolar : Fuzzer Point
pointPolar =
    polar
        |> Fuzz.map Point.fromPolar


cartesian : Fuzzer Cartesian
cartesian =
    pair niceFloat niceFloat
        |> Fuzz.map Cartesian.from


polar : Fuzzer Polar
polar =
    pair niceFloat niceFloat
        |> Fuzz.map Polar.from
