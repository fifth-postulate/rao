module Rao exposing (compat, view)

import Fraction exposing (Fraction)
import Html exposing (Html)


view : List Fraction -> Html msg
view angles =
    angles
        |> compat
        |> List.map viewVectorType
        |> Html.div []


viewVectorType : List Int -> Html msg
viewVectorType weights =
    let
        comma =
            Html.text ", "
    in
    weights
        |> List.map (String.fromInt >> Html.text)
        |> List.intersperse comma
        |> Html.pre []
        |> List.singleton
        |> Html.div []


compat : List Fraction -> List (List Int)
compat angles =
    let
        go : Fraction -> List Fraction -> List (List Int)
        go remainingSum ts =
            case ts of
                [] ->
                    if remainingSum == Fraction.zero then
                        [ [] ]

                    else
                        []

                v :: vs ->
                    let
                        max =
                            Fraction.divide remainingSum v
                                |> Maybe.withDefault Fraction.zero
                                |> Fraction.floor

                        adjoin : Int -> List (List Int)
                        adjoin n =
                            let
                                rs =
                                    Fraction.subtract remainingSum (Fraction.scale n v)
                            in
                            go rs vs
                                |> List.map (\ws -> n :: ws)
                    in
                    List.range 0 max
                        |> List.reverse
                        |> List.concatMap adjoin

        two =
            Fraction.create 2 1 |> Maybe.withDefault Fraction.one
    in
    go two angles
