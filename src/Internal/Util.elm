module Internal.Util exposing (listFind, reverseNonEmpty, timeDiff)

import Time exposing (Posix)


timeDiff : Posix -> Posix -> Int
timeDiff a b =
    abs <|
        Time.posixToMillis a
            - Time.posixToMillis b


reverseNonEmpty : ( a, List a ) -> ( a, List a )
reverseNonEmpty ( a, list ) =
    let
        revapp : ( List a, a, List a ) -> ( a, List a )
        revapp ( ls, c, rs ) =
            case rs of
                [] ->
                    ( c, ls )

                r :: rss ->
                    revapp ( c :: ls, r, rss )
    in
    revapp ( [], a, list )


listFind : (a -> Bool) -> List a -> Maybe a
listFind pred list =
    case list of
        x :: xs ->
            if pred x then
                Just x

            else
                listFind pred xs

        [] ->
            Nothing
