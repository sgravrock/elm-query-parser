module ParseValidQueryTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import QueryString exposing (parseValidQuery, Param(..))


suite : Test
suite =
    describe "parseValidQuery"
        [ test "ignores invalid keys" <|
            \_ ->
                let
                    expected = [("foo", "bar"), ("baz", "qux")]
                    actual = parseValidQuery "?foo=bar&%=bogus&baz=qux"
                in
                    Expect.equal expected actual
        , test "ignores invalid values" <|
            \_ ->
                let
                    expected = [("foo", "bar"), ("baz", "qux")]
                    actual = parseValidQuery "?foo=bar&bogus%=5&baz=qux"
                in
                    Expect.equal expected actual
        ]
