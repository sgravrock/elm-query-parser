module StringUtils exposing (splitOnce)

splitOnce : String -> String -> (String, String)
splitOnce delim s =
    case String.split delim s of
        x::xs -> (x, String.join delim xs)
        {- This can't happen because String.split always returns at least
           one element, but good luck telling Elm that.
        -}
        [] -> ("", "")
