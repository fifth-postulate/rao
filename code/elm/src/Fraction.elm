module Fraction exposing (Fraction, add, create, divide, floor, fromInt, invert, isZero, multiply, negate, one, scale, subtract, toFloat, toString, two, zero)


type Fraction
    = Fraction Int Int


zero : Fraction
zero =
    fromInt 0


one : Fraction
one =
    fromInt 1


two : Fraction
two =
    fromInt 2


fromInt : Int -> Fraction
fromInt n =
    Fraction n 1


create : Int -> Int -> Maybe Fraction
create numerator denominator =
    if denominator == 0 then
        Nothing

    else
        Just (create_ numerator denominator)


create_ : Int -> Int -> Fraction
create_ numerator denominator =
    let
        t =
            abs numerator

        n =
            abs denominator

        s =
            sign numerator * sign denominator

        ( g, _, _ ) =
            egcd t n
    in
    Fraction (s * t // g) (n // g)


toString : Fraction -> String
toString a =
    case a of
        Fraction n d ->
            String.fromInt n ++ "/" ++ String.fromInt d


sign : Int -> Int
sign n =
    case compare n 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1


egcd : Int -> Int -> ( Int, Int, Int )
egcd a b =
    let
        -- invariant: x = u * a + v * b && y = s * a + t * b
        go : Int -> Int -> Int -> Int -> Int -> Int -> ( Int, Int, Int )
        go x y u v s t =
            if y == 0 then
                ( x, u, v )

            else
                let
                    q =
                        x // y
                in
                go y (x - q * y) s t (u - q * s) (v - q * t)
    in
    go (abs a) (abs b) (sign a) 0 0 (sign b)


add : Fraction -> Fraction -> Fraction
add a b =
    case ( a, b ) of
        ( Fraction na da, Fraction nb db ) ->
            create_ (db * na + da * nb) (da * db)


multiply : Fraction -> Fraction -> Fraction
multiply a b =
    case ( a, b ) of
        ( Fraction na da, Fraction nb db ) ->
            create_ (na * nb) (da * db)


negate : Fraction -> Fraction
negate a =
    case a of
        Fraction n d ->
            Fraction (Basics.negate n) d


invert : Fraction -> Maybe Fraction
invert a =
    case a of
        Fraction n d ->
            if n /= 0 then
                Just <| Fraction d n

            else
                Nothing


subtract : Fraction -> Fraction -> Fraction
subtract a b =
    add a (negate b)


divide : Fraction -> Fraction -> Maybe Fraction
divide a b =
    b
        |> invert
        |> Maybe.map (multiply a)


floor : Fraction -> Int
floor a =
    case a of
        Fraction n d ->
            n // d


scale : Int -> Fraction -> Fraction
scale t v =
    case v of
        Fraction n d ->
            create_ (t * n) d


toFloat : Fraction -> Float
toFloat v =
    case v of
        Fraction n d ->
            Basics.toFloat n / Basics.toFloat d


isZero : Fraction -> Bool
isZero v =
    case v of
        Fraction n _ ->
            n == 0
