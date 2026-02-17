module Polygon.Point.Polar exposing (Polar, from, rotate, scale, to)


type Polar
    = Polar { r : Float, angle : Float }


from : ( Float, Float ) -> Polar
from ( r, angle ) =
    Polar { r = r, angle = angle }


to : Polar -> ( Float, Float )
to (Polar { r, angle }) =
    ( r, angle )


scale : Float -> Polar -> Polar
scale s (Polar polar) =
    Polar { polar | r = s * polar.r }


rotate : Float -> Polar -> Polar
rotate da (Polar polar) =
    Polar { polar | angle = da + polar.angle }
