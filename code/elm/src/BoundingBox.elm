module BoundingBox exposing (BoundingBox, boundingBox)


type alias BoundingBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


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
