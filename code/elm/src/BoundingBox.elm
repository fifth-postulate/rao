module BoundingBox exposing (BoundingBox, boundingBox, box, expand, squareUp)

import BoundingBox.Interval as Interval exposing (Interval, extend)


type alias BoundingBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


box : Float -> Float -> Float -> Float -> BoundingBox
box x y width height =
    { x = x, y = y, width = width, height = height }


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
            box (Interval.min xs) (Interval.min ys) (Interval.size xs) (Interval.size ys)
    in
    case pss of
        ( x, y ) :: rest ->
            go (Interval.point x) (Interval.point y) rest
                |> toBBox

        [] ->
            ( Interval.point 0, Interval.point 0 )
                |> toBBox


expand : Float -> BoundingBox -> BoundingBox
expand delta bbox =
    { bbox
        | x = bbox.x - delta / 2
        , y = bbox.y - delta / 2
        , width = bbox.width + delta
        , height = bbox.height + delta
    }


squareUp : BoundingBox -> BoundingBox
squareUp bbox =
    let
        side =
            max bbox.width bbox.height
    in
    { bbox
        | x = bbox.x - ((side - bbox.width) / 2)
        , y = bbox.y - ((side - bbox.height) / 2)
        , width = side
        , height = side
    }
