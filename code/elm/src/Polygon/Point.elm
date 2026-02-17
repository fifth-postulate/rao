module Polygon.Point exposing (Point, fromCartesian, fromPolar, rotate, scale, toCartesian, toPolar)

import Polygon.Point.Cartesian as Cartesian exposing (Cartesian)
import Polygon.Point.Polar as Polar exposing (Polar)


type Point
    = Point Cartesian Polar


fromCartesian : Cartesian -> Point
fromCartesian cartesian =
    Point cartesian (cartesian2Polar cartesian)


toCartesian : Point -> Cartesian
toCartesian (Point cartesian _) =
    cartesian


fromPolar : Polar -> Point
fromPolar polar =
    Point (polar2cartesian polar) polar


toPolar : Point -> Polar
toPolar (Point _ polar) =
    polar


cartesian2Polar : Cartesian -> Polar
cartesian2Polar cartesian =
    let
        x =
            Cartesian.x cartesian

        y =
            Cartesian.y cartesian

        r =
            sqrt (x ^ 2 + y ^ 2)

        angle =
            atan2 y x
    in
    Polar.from ( r, angle )


polar2cartesian : Polar -> Cartesian
polar2cartesian polar =
    let
        r =
            Polar.r polar

        angle =
            Polar.a polar

        x =
            r * cos angle

        y =
            r * sin angle
    in
    Cartesian.from ( x, y )


scale : Float -> Point -> Point
scale s (Point cartesian polar) =
    Point (Cartesian.scale s cartesian) (Polar.scale s polar)


rotate : Float -> Point -> Point
rotate angle point =
    point
        |> toPolar
        |> Polar.rotate angle
        |> fromPolar
