module ParseQueryAsDictTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict
import QueryString exposing (parseQueryAsDict, Param(..))


suite : Test
suite =
    describe "parseQueryAsDict"
        [ test "stores keys and values" <|
            \_ ->
                let
                    expected = Dict.fromList [("foo", "bar"), ("baz", "qux")]
                    actual = parseQueryAsDict "?foo=bar&baz=qux"
                in
                    Expect.equal expected actual
        , test "ignores values corresponding to duplicate keys" <|
            \_ ->
                let
                    expected = Dict.fromList [("foo", "bar")]
                    actual = parseQueryAsDict "?foo=bar&foo=baz"
                in
                    Expect.equal expected actual
        ]
