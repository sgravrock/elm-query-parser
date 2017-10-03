module ParseQueryTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import QueryString exposing (parseQuery, Param(..))


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
        , test "URL-decodes the components" <|
            \_ ->
                let
                    expected = [ ValidParam "this & that" "some stuff"
                               , ValidParam "x" "y"
                               ]
                    actual = parseQuery "?this%20%26%20that=some%20stuff&x=y"
                in
                    Expect.equal expected actual
        , test "Treats non-decodable keys as invalid" <|
            \_ ->
                let
                    expected = [InvalidParam Nothing (Just "v")]
                    actual = parseQuery "?%=v"
                in
                    Expect.equal expected actual
        , test "Treats non-decodable values as invalid" <|
            \_ ->
                let
                    expected = [InvalidParam (Just "k") Nothing]
                    actual = parseQuery "?k=%"
                in
                    Expect.equal expected actual
        ]
