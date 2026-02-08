module Compatible exposing (main)

import Browser
import Fraction exposing (Fraction)
import Html exposing (Html)
import Rao


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { angles : List Fraction
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        a =
            Fraction.create 1 2 |> Maybe.withDefault Fraction.zero

        b =
            Fraction.create 3 4 |> Maybe.withDefault Fraction.zero
    in
    ( { angles = [ a, b, a, a, b ] }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewInput model
        , viewOutput model
        ]


viewInput : Model -> Html Msg
viewInput model =
    let
        comma =
            Html.span [] [ Html.text ", " ]
    in
    model.angles
        |> List.indexedMap viewAngle
        |> List.intersperse comma
        |> Html.div []


viewAngle : Int -> Fraction -> Html Msg
viewAngle index value =
    Html.span []
        [ Html.text "a"
        , Html.sub [] [ Html.text (String.fromInt (index + 1)) ]
        , Html.text "="
        , Html.text (Fraction.toString value)
        ]


viewOutput : Model -> Html Msg
viewOutput model =
    let
        ws =
            Rao.compat model.angles
    in
    model.angles
        |> Rao.compat
        |> List.map viewVectorType
        |> Html.div []


viewVectorType : List Int -> Html Msg
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


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
