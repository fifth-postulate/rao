module Polygon.Point.Cartesian exposing (Cartesian, from, scale, to, translate, x, y)


type Cartesian
    = Cartesian { x : Float, y : Float }


x : Cartesian -> Float
x (Cartesian cartesian) =
    cartesian.x


y : Cartesian -> Float
y (Cartesian cartesian) =
    cartesian.y


from : ( Float, Float ) -> Cartesian
from ( px, py ) =
    Cartesian { x = px, y = py }


to : Cartesian -> ( Float, Float )
to (Cartesian cartesian) =
    ( cartesian.x, cartesian.y )


translate : ( Float, Float ) -> Cartesian -> Cartesian
translate ( dx, dy ) (Cartesian cartesian) =
    Cartesian { cartesian | x = cartesian.x + dx, y = cartesian.y + dy }


scale : Float -> Cartesian -> Cartesian
scale r (Cartesian cartesian) =
    Cartesian { cartesian | x = r * cartesian.x, y = r * cartesian.y }
