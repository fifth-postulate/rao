module BoundingBox.Interval exposing (Interval, extend, max, min, point, size)

import Basics


type Interval
    = Interval
        { left : Float
        , right : Float
        }


min : Interval -> Float
min (Interval { left }) =
    left


max : Interval -> Float
max (Interval { right }) =
    right


size : Interval -> Float
size (Interval { left, right }) =
    right - left


point : Float -> Interval
point v =
    Interval { left = v, right = v }


extend : Float -> Interval -> Interval
extend z (Interval { left, right }) =
    Interval { left = Basics.min z left, right = Basics.max z right }
