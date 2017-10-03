module ParseTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import QueryString exposing (parse, Param(..))


suite : Test
suite =
    describe "parse"
        [ test "parses an empty query string" <|
            \_ -> Expect.equal [] (parse "?")
        , test "returns params in order" <|
            \_ -> 
                let
                    expected = [ValidParam "foo" "bar", ValidParam "baz" "qux"]
                    actual = parse "?foo=bar&baz=qux"
                in
                    Expect.equal expected actual
        , test "ignores missing ? prefix" <|
            \_ ->
                let
                    expected = [ValidParam "foo" "bar", ValidParam "baz" "qux"]
                    actual = parse "foo=bar&baz=qux"
                in
                    Expect.equal expected actual
        , test "treats nonexistent = as an empty value" <|
            \_ ->
                let
                    expected = [ValidParam "foo" "", ValidParam "baz" "qux"]
                    actual = parse "?foo&baz=qux"
                in
                    Expect.equal expected actual
        , test "handles duplicates" <|
            \_ ->
                let
                    expected = [ValidParam "foo" "a", ValidParam "foo" "b"]
                    actual = parse "?foo=a&foo=b"
                in
                    Expect.equal expected actual
        , test "handles embedded =" <|
            \_ ->
                let
                    expected = [ValidParam "foo" "a=b=c", ValidParam "bar" "d"]
                    actual = parse "?foo=a=b=c&bar=d"
                in
                    Expect.equal expected actual
        , test "URL-decodes the components" <|
            \_ ->
                let
                    expected = [ ValidParam "this & that" "some stuff"
                               , ValidParam "x" "y"
                               ]
                    actual = parse "?this%20%26%20that=some%20stuff&x=y"
                in
                    Expect.equal expected actual
        , test "Treats empty keys as invalid" <|
            \_ ->
                let
                    expected = [InvalidParam Nothing (Just "v")]
                    actual = parse "?=v"
                in
                    Expect.equal expected actual
        , test "Treats non-decodable keys as invalid" <|
            \_ ->
                let
                    expected = [InvalidParam Nothing (Just "v")]
                    actual = parse "?%=v"
                in
                    Expect.equal expected actual
        , test "Treats non-decodable values as invalid" <|
            \_ ->
                let
                    expected = [InvalidParam (Just "k") Nothing]
                    actual = parse "?k=%"
                in
                    Expect.equal expected actual
        ]
