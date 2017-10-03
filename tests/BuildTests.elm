module BuildTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, string, tuple, conditional)
import Test exposing (Test, describe, fuzz)
import Dict
import QueryString exposing (parseValid, build, Param(..))

type alias Conditional a =
    { retries: Int
    , fallback: a -> a
    , condition: a -> Bool
    }

hasNonEmptyKey : Conditional (String, String)
hasNonEmptyKey =
    { retries = 10
    , fallback = \(k,v) -> (k ++ "x", v)
    , condition = \(k, v) -> k /= ""
    }


makeQueryParam : Fuzzer (String, String)
makeQueryParam =
    let
        allValues = tuple (string, string)
    in
        conditional hasNonEmptyKey allValues

makeQueryList : Fuzzer (List (String, String))
makeQueryList = list makeQueryParam

suite : Test
suite =
    describe "build"
        [ fuzz makeQueryList "(parseValid . build) is identity"
            <| \xs  -> Expect.equal xs (parseValid (build xs))
        ]
