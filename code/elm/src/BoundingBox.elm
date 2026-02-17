module BoundingBox exposing (BoundingBox, boundingBox, expand, squareUp)

import BoundingBox.Interal exposing (Interval, extend)


type alias BoundingBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


expand : Float -> BoundingBox -> BoundingBox
expand delta box =
    { box
        | x = box.x - delta / 2
        , y = box.y - delta / 2
        , width = box.width + delta
        , height = box.height + delta
    }


squareUp : BoundingBox -> BoundingBox
squareUp box =
    let
        side =
            max box.width box.height
    in
    { box
        | x = box.x - ((side - box.width) / 2)
        , y = box.y - ((side - box.height) / 2)
        , width = side
        , height = side
    }


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
            { x = Interval.min xs
            , y = Interval.min ys
            , width = Interval.size xs
            , height = Interval.size ys
            }
    in
    case pss of
        ( x, y ) :: rest ->
            go (Interval.point x) (Interval.point y) rest
                |> toBBox

        [] ->
            ( Interval.point 0, Interval.point 0 )
                |> toBBox
