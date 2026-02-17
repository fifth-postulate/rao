module Util exposing (rotate, uncurry, zip)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


zip : List a -> List b -> List ( a, b )
zip xs ys =
    let
        go : List ( a, b ) -> List a -> List b -> List ( a, b )
        go acc us vs =
            case ( us, vs ) of
                ( u :: uss, v :: vss ) ->
                    go (( u, v ) :: acc) uss vss

                _ ->
                    List.reverse acc
    in
    go [] xs ys


rotate : List a -> List a
rotate us =
    let
        hs =
            List.take 1 us

        ts =
            List.drop 1 us
    in
    List.append ts hs
