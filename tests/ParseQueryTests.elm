module ParseQueryTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import QueryParser exposing (parseQuery, Param(..))


suite : Test
suite =
    describe "parseQuery"
        [ test "returns params in order" <|
            \_ -> 
                let
                    expected = [ValidParam "foo" "bar", ValidParam "baz" "qux"]
                    actual = parseQuery "?foo=bar&baz=qux"
                in
                    Expect.equal expected actual
        , test "treats nonexistent = as an empty value" <|
            \_ ->
                let
                    expected = [ValidParam "foo" "", ValidParam "baz" "qux"]
                    actual = parseQuery "?foo&baz=qux"
                in
                    Expect.equal expected actual
        , test "handles duplicates" <|
            \_ ->
                let
                    expected = [ValidParam "foo" "a", ValidParam "foo" "b"]
                    actual = parseQuery "?foo=a&foo=b"
                in
                    Expect.equal expected actual
        , test "handles embedded =" <|
            \_ ->
                let
                    expected = [ValidParam "foo" "a=b=c", ValidParam "bar" "d"]
                    actual = parseQuery "?foo=a=b=c&bar=d"
                in
                    Expect.equal expected actual
        ]
