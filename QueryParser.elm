module QueryParser exposing (parseQuery, Param(..))
import StringUtils

type Param
    = InvalidParam (Maybe String) (Maybe String)
    | ValidParam String String


parseQuery : String -> List Param
parseQuery s =
    let
        qs = String.dropLeft 1 s
        pairs = String.split "&" qs
    in
        List.map parsePair pairs

parsePair : String -> Param
parsePair s =
    let
        (k, v) = StringUtils.splitOnce "=" s
    in
        ValidParam k v
