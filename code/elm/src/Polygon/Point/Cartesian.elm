module Polygon.Point.Cartesian exposing (Cartesian, from, scale, to, translate)


type Cartesian
    = Cartesian { x : Float, y : Float }


from : ( Float, Float ) -> Cartesian
from ( x, y ) =
    Cartesian { x = x, y = y }


to : Cartesian -> ( Float, Float )
to (Cartesian { x, y }) =
    ( x, y )


translate : ( Float, Float ) -> Cartesian -> Cartesian
translate ( dx, dy ) (Cartesian { x, y }) =
    Cartesian { x = x + dx, y = y + dy }


scale : Float -> Cartesian -> Cartesian
scale r (Cartesian { x, y }) =
    Cartesian { x = r * x, y = r * y }
