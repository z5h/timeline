module Internal.Util exposing
    ( addMilliseconds
    , after
    , listFind
    , listFindSequence
    , listPairs
    , niceBezierString
    , reverseNonEmpty
    , timeDiff
    )

import Time exposing (Posix)


addMilliseconds : Int -> Posix -> Posix
addMilliseconds ms p =
    Time.posixToMillis p |> (+) ms |> Time.millisToPosix


after : Posix -> Posix -> Bool
after this other =
    Time.posixToMillis other > Time.posixToMillis this


timeDiff : Posix -> Posix -> Int
timeDiff a b =
    abs <|
        Time.posixToMillis a
            - Time.posixToMillis b


reverseNonEmpty : ( a, List a ) -> ( a, List a )
reverseNonEmpty ( a, list ) =
    let
        rev : ( List a, a, List a ) -> ( a, List a )
        rev ( ls, c, rs ) =
            case rs of
                [] ->
                    ( c, ls )

                r :: rss ->
                    rev ( c :: ls, r, rss )
    in
    rev ( [], a, list )


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


listPairs : List a -> List ( a, a )
listPairs =
    let
        listPairsHelper : List ( a, a ) -> List a -> List ( a, a )
        listPairsHelper reversedResult list =
            case list of
                [] ->
                    reversedResult

                _ :: [] ->
                    reversedResult

                a :: b :: rest ->
                    listPairsHelper (( a, b ) :: reversedResult) (b :: rest)
    in
    listPairsHelper [] >> List.reverse


listFindSequence : (a -> a -> Bool) -> List a -> Maybe ( a, a )
listFindSequence pred list =
    case list of
        x :: y :: rest ->
            if pred x y then
                Just ( x, y )

            else
                listFindSequence pred (y :: rest)

        _ ->
            Nothing


niceBezierString : String
niceBezierString =
    "cubic-bezier(0.78,0,0.22,1)"
