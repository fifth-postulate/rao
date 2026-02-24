module Polygon exposing (Polygon, fromAngles, view)

import BoundingBox exposing (BoundingBox)
import Fraction exposing (Fraction)
import Polygon.Point as Point exposing (Point)
import Polygon.Point.Cartesian as Cartesian
import Polygon.Point.Polar as Polar exposing (Polar)
import Svg exposing (Svg)
import Svg.Attributes as Attribute
import Util exposing (rotate, uncurry, zip)


type Polygon
    = Points (List Point)


fromAngles : List Fraction -> Polygon
fromAngles angles =
    Points (vertexPoints angles)


vertexPoints : List Fraction -> List Point
vertexPoints angles =
    let
        alphas =
            angles
                |> List.map (\a -> Fraction.divide a Fraction.two |> Maybe.withDefault Fraction.zero)

        betas =
            zip alphas (rotate alphas)
                |> List.map (uncurry Fraction.add)
                |> List.map (Fraction.subtract Fraction.one)

        go : List Point -> Polar -> List Fraction -> List Fraction -> List Point
        go acc polar us vs =
            case ( us, vs ) of
                ( u :: uss, v :: vss ) ->
                    let
                        alpha =
                            pi * Fraction.toFloat u

                        beta =
                            pi * Fraction.toFloat v

                        x =
                            Polar.r polar * tan alpha / (tan alpha + tan beta)

                        y =
                            x * tan beta

                        r_ =
                            sqrt (x ^ 2 + y ^ 2)

                        point =
                            polar
                                |> Point.fromPolar
                    in
                    go
                        (point :: acc)
                        (Polar.from ( r_, Polar.a polar + beta ))
                        uss
                        vss

                _ ->
                    List.reverse acc
    in
    go [] (Polar.from ( 1.0, 0.0 )) alphas betas


view : Polygon -> ( Svg msg, BoundingBox )
view polygon =
    case polygon of
        Points points ->
            viewPoints points


viewPoints : List Point -> ( Svg msg, BoundingBox )
viewPoints points =
    let
        toPair : Point -> ( Float, Float )
        toPair =
            Point.toCartesian >> Cartesian.to

        pointToString : Point -> String
        pointToString point =
            let
                ( x, y ) =
                    toPair point
            in
            String.fromFloat x ++ "," ++ String.fromFloat y

        ps =
            points
                |> List.map pointToString
                |> String.join " "
    in
    ( Svg.polygon
        [ Attribute.points ps
        ]
        []
    , BoundingBox.boundingBox (List.map toPair points)
    )
