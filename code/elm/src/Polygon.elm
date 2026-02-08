module Polygon exposing (BoundingBox, Polygon, fromAngles, view)

import Fraction exposing (Fraction, subtract)
import Svg exposing (Svg)
import Svg.Attributes as Attribute


type Polygon
    = Angles (List Fraction)


type alias BoundingBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


fromAngles : List Fraction -> Polygon
fromAngles =
    Angles


view : Polygon -> ( Svg msg, BoundingBox )
view polygon =
    case polygon of
        Angles angles ->
            viewAngles angles


viewAngles : List Fraction -> ( Svg msg, BoundingBox )
viewAngles angles =
    let
        pairToString : ( Float, Float ) -> String
        pairToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y

        points =
            angles
                |> vertexPoints

        ps =
            points
                |> List.map pairToString
                |> String.join " "
    in
    ( Svg.polygon
        [ Attribute.points ps
        ]
        []
    , boundingBox points
    )


type alias Interval =
    { left : Float
    , right : Float
    }


extend : Float -> Interval -> Interval
extend z interval =
    { interval | left = min z interval.left, right = max z interval.right }


boundingBox : List ( Float, Float ) -> BoundingBox
boundingBox pss =
    let
        go : Interval -> Interval -> List ( Float, Float ) -> ( Interval, Interval )
        go xInterval yInterval ps =
            case ps of
                [] ->
                    ( xInterval, yInterval )

                ( x, y ) :: rest ->
                    go (extend x xInterval) (extend y yInterval) rest

        toBBox : ( Interval, Interval ) -> BoundingBox
        toBBox ( xs, ys ) =
            { x = xs.left
            , y = ys.left
            , width = xs.right - xs.left
            , height = ys.right - ys.left
            }
    in
    case pss of
        ( x, y ) :: rest ->
            go { left = x, right = x } { left = y, right = y } rest
                |> toBBox

        [] ->
            ( { left = 0, right = 0 }, { left = 0, right = 0 } )
                |> toBBox


vertexPoints : List Fraction -> List ( Float, Float )
vertexPoints angles =
    let
        two =
            Fraction.create 2 1
                |> Maybe.withDefault Fraction.zero

        alphas =
            angles
                |> List.map (\a -> Fraction.divide a two |> Maybe.withDefault Fraction.zero)

        betas =
            zip alphas (rotate alphas)
                |> List.map (uncurry Fraction.add)
                |> List.map (Fraction.subtract Fraction.one)

        go : List ( Float, Float ) -> Float -> Float -> List Fraction -> List Fraction -> List ( Float, Float )
        go acc r angle us vs =
            case ( us, vs ) of
                ( u :: uss, v :: vss ) ->
                    let
                        alpha =
                            pi * Fraction.toFloat u

                        beta =
                            pi * Fraction.toFloat v

                        x =
                            r * tan alpha / (tan alpha + tan beta)

                        y =
                            x * tan beta

                        r_ =
                            sqrt (x ^ 2 + y ^ 2)
                    in
                    go
                        (( r * cos angle, r * sin angle ) :: acc)
                        r_
                        (angle + beta)
                        uss
                        vss

                _ ->
                    List.reverse acc
    in
    go [] 1.0 0.0 alphas betas


zip : List a -> List b -> List ( a, b )
zip xs ys =
    let
        go : List ( a, b ) -> List a -> List b -> List ( a, b )
        go acc us vs =
            case ( us, vs ) of
                ( u :: uss, v :: vss ) ->
                    go (( u, v ) :: acc) uss vss

                _ ->
                    List.reverse acc
    in
    go [] xs ys


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


rotate : List a -> List a
rotate us =
    let
        hs =
            List.take 1 us

        ts =
            List.drop 1 us
    in
    List.append ts hs
