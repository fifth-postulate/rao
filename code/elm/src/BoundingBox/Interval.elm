module BoundingBox.Interval exposing (Interval, extend, max, min, point, size)

import Basics


type alias Interval =
    { left : Float
    , right : Float
    }


min : Interval -> Float
min { left } =
    left


max : Interval -> Float
max { right } =
    right


size : Interval -> Float
size { left, right } =
    right - left


point : Float -> Interval
point v =
    { left = v, right = v }


extend : Float -> Interval -> Interval
extend z interval =
    { interval | left = Basics.min z interval.left, right = Basics.max z interval.right }
