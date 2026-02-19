module Algebra.Vector exposing (Vector, add, dimension, dot, fromList, index, scale, subtract, zero)

import Fraction exposing (Fraction)
import Util exposing (uncurry, zip)


type Vector
    = Vector (List Fraction)


fromList : List Fraction -> Vector
fromList =
    Vector


zero : Int -> Vector
zero dim =
    Fraction.zero
        |> List.repeat (max dim 0)
        |> fromList


dimension : Vector -> Int
dimension (Vector coordinates) =
    List.length coordinates


add : Vector -> Vector -> Vector
add (Vector left) (Vector right) =
    zip left right
        |> List.map (uncurry Fraction.add)
        |> fromList


subtract : Vector -> Vector -> Vector
subtract left right =
    add left (scale (Fraction.negate Fraction.one) right)


scale : Fraction -> Vector -> Vector
scale s (Vector coordinates) =
    coordinates
        |> List.map (Fraction.multiply s)
        |> fromList


dot : Vector -> Vector -> Fraction
dot (Vector left) (Vector right) =
    zip left right
        |> List.map (uncurry Fraction.multiply)
        |> List.foldr Fraction.add Fraction.zero


index : Int -> Vector -> Maybe Fraction
index n (Vector vector) =
    if n < 0 then
        Nothing

    else
        vector
            |> List.drop n
            |> List.head
