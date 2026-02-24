module TestUtils exposing (toBeTrue)

import Expect exposing (Expectation)


toBeTrue : Bool -> Expectation
toBeTrue b =
    if b then
        Expect.pass

    else
        Expect.fail "expected to be True"
