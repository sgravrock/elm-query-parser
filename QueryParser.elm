module QueryParser exposing (parseQuery, Param(..))
import Http
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
        (ek, ev) = StringUtils.splitOnce "=" s
    in
        case (Http.decodeUri ek, Http.decodeUri ev) of
            (Just k, Just v) -> ValidParam k v
            (maybeK, maybeV) -> InvalidParam maybeK maybeV
