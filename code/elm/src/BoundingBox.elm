module BoundingBox exposing (BoundingBox, boundingBox, expand, squareUp)


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
