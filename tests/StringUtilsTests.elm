module StringUtilsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import StringUtils exposing (splitOnce)


suite : Test
suite =
    describe "splitOnce"
        [ test "splits on the first occurrence" <|
            \_ ->  Expect.equal ("a", "b=c=d") (splitOnce "=" "a=b=c=d")
        , test "handles an empty second part" <|
            \_ ->  Expect.equal ("a", "") (splitOnce "=" "a=")
        , test "returns an empty value when there is no delimiter" <|
            \_ ->  Expect.equal ("bob", "") (splitOnce "=" "bob")
        ]
