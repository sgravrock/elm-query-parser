module QueryString exposing ( parse, parseValid, parseToDict
                            , Param(..)
                            )
import Http
import Dict
import StringUtils

type Param
    = InvalidParam (Maybe String) (Maybe String)
    | ValidParam String String


parse : String -> List Param
parse s =
    let
        qs = String.dropLeft 1 s
        pairs = String.split "&" qs
    in
        List.map parsePair pairs


parseValid : String -> List (String, String)
parseValid s = List.filterMap unpackValid <| parse s

parseToDict : String -> Dict.Dict String String
parseToDict s =
    let
        pairs = parseValid s
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
