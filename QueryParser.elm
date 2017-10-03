module QueryParser exposing ( parseQuery, parseValidQuery, parseQueryAsDict
                            , Param(..)
                            )
import Http
import Dict
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


parseValidQuery : String -> List (String, String)
parseValidQuery s = List.filterMap unpackValid <| parseQuery s

parseQueryAsDict : String -> Dict.Dict String String
parseQueryAsDict s =
    let
        pairs = parseValidQuery s
    in
        List.foldr (\(k, v) -> Dict.insert k v) Dict.empty pairs

parsePair : String -> Param
parsePair s =
    let
        (ek, ev) = StringUtils.splitOnce "=" s
    in
        case (Http.decodeUri ek, Http.decodeUri ev) of
            (Just k, Just v) -> ValidParam k v
            (maybeK, maybeV) -> InvalidParam maybeK maybeV

unpackValid : Param -> Maybe (String, String)
unpackValid p =
    case p of
        InvalidParam _ _ -> Nothing
        ValidParam k v -> Just (k, v)
