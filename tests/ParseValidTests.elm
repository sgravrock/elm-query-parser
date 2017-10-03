module ParseValidTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import QueryString exposing (parseValid, Param(..))


suite : Test
suite =
    describe "parseValid"
        [ test "ignores invalid keys" <|
            \_ ->
                let
                    expected = [("foo", "bar"), ("baz", "qux")]
                    actual = parseValid "?foo=bar&%=bogus&baz=qux"
                in
                    Expect.equal expected actual
        , test "ignores invalid values" <|
            \_ ->
                let
                    expected = [("foo", "bar"), ("baz", "qux")]
                    actual = parseValid "?foo=bar&bogus%=5&baz=qux"
                in
                    Expect.equal expected actual
        ]
