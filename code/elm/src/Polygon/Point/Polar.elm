module Polygon.Point.Polar exposing (Polar, a, from, r, rotate, scale, to)


type Polar
    = Polar { radius : Float, angle : Float }


from : ( Float, Float ) -> Polar
from ( radius, angle ) =
    Polar { radius = radius, angle = angle }


to : Polar -> ( Float, Float )
to (Polar { radius, angle }) =
    ( radius, angle )


r : Polar -> Float
r (Polar polar) =
    polar.radius


a : Polar -> Float
a (Polar polar) =
    polar.angle


scale : Float -> Polar -> Polar
scale s (Polar polar) =
    Polar { polar | radius = s * polar.radius }


rotate : Float -> Polar -> Polar
rotate da (Polar polar) =
    Polar { polar | angle = da + polar.angle }
