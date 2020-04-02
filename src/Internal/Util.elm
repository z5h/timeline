module Internal.Util exposing
    ( addMilliseconds
    , after
    , keyframes
    , listFind
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


keyframes : String
keyframes =
    """
@keyframes z5h_timeline_fade_in_### {
    from { opacity: 0 }
    to { opacity: 1 }
}

@keyframes z5h_timeline_fade_out_### {
    from { opacity: 1 }
    to { opacity: 0 }
}

@keyframes z5h_timeline_slide_left_out_### {
    from { transform: translateX(0%) }
    to { transform: translateX(-100%) }

}

@keyframes z5h_timeline_slide_left_in_### {
    from { transform: translateX(-100%) }
    to { transform: translateX(0%) }
}
"""
        |> (\s ->
                String.join "\n"
                    [ s |> String.replace "###" "0"
                    , s |> String.replace "###" "1"
                    ]
           )
