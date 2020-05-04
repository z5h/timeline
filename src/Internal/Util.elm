module Internal.Util exposing
    ( addMilliseconds
    , after
    , keyframes
    , listFind
    , listFindSequence
    , listPairs
    , listUntilInclusive
    , niceBezierString
    , reverseNonEmpty
    , timeDiff
    , timeUntil
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


timeUntil : Posix -> Posix -> Int
timeUntil later earlier =
    Time.posixToMillis later
        - Time.posixToMillis earlier
        |> max 0


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


listPairs : List a -> List ( a, Maybe a )
listPairs =
    let
        listPairsHelper : List ( a, Maybe a ) -> List a -> List ( a, Maybe a )
        listPairsHelper reversedResult list =
            case list of
                [] ->
                    reversedResult

                a :: [] ->
                    ( a, Nothing ) :: reversedResult

                a :: b :: rest ->
                    listPairsHelper (( a, Just b ) :: reversedResult) (b :: rest)
    in
    listPairsHelper [] >> List.reverse


listUntilInclusive : (a -> Bool) -> List a -> List a
listUntilInclusive pred =
    let
        listUntilHelper : List a -> List a -> List a
        listUntilHelper reversedResult list =
            case list of
                [] ->
                    reversedResult

                a :: rest ->
                    if not (pred a) then
                        listUntilHelper (a :: reversedResult) rest

                    else
                        a :: reversedResult
    in
    listUntilHelper []


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


keyframes : String
keyframes =
    """
/*=== fade_in ===*/
.z5h_timeline__fade_in.start {
    opacity: 0;
    pointer-events: none;
    overflow: hidden;
}

.z5h_timeline__fade_in.in-progress {
    pointer-events: none;
}

@keyframes z5h_timeline__fade_in__### {
    from {
        opacity: 0;
        pointer-events: none;
    }
    to {
        opacity: 1;
        pointer-events: none;
    }
}

/*=== scale_in_y ===*/
.z5h_timeline__scale_in_y.start {
    opacity: 0;
    max-height: 0px;
    pointer-events: none;
    overflow: hidden;
}

.z5h_timeline__scale_in_y.in-progress {
    pointer-events: none;
    overflow: hidden;
}

@keyframes z5h_timeline__scale_in_y__### {
    from {
        opacity: 0;
        max-height: 0px;
        pointer-events: none;
        overflow: hidden;
    }
    to {
        opacity: 1;
        max-height: 1080px;
        pointer-events: none;
        overflow: hidden;
    }
}

/*=== scale_in_x ===*/
.z5h_timeline__scale_in_x.start {
    opacity: 0;
    max-width: 0px;
    pointer-events: none;
    overflow: hidden;
}

.z5h_timeline__scale_in_x.in-progress {
    pointer-events: none;
    overflow: hidden;
}

@keyframes z5h_timeline__scale_in_x__### {
    from {
        opacity: 0;
        max-width: 0px;
        pointer-events: none;
        overflow: hidden;
    }
    to {
        opacity: 1;
        max-width: 1920px;
        pointer-events: none;
        overflow: hidden;
    }
}

/*=== slide_in_from_left ===*/
.z5h_timeline__slide_in_from_left {
    transform: translateX(-100%);
}

@keyframes z5h_timeline__slide_in_from_left__### {
    from { transform: translateX(-100%); }
    to { transform: translateX(0%); }
}

/*=== slide_in_from_right ===*/
.z5h_timeline__slide_in_from_right {
    transform: translateX(100%);
}

@keyframes z5h_timeline__slide_in_from_right__### {
    from { transform: translateX(100%); }
    to { transform: translateX(0%); }
}

/*=== pulse ===*/
@keyframes z5h_timeline__pulse__### {
    from { transform: scale3d(1,    1,    1   ); }
    50%  { transform: scale3d(1.05, 1.05, 1.05); }
    to   { transform: scale3d(1,    1,    1   ); }
}

/*=== flash ===*/
@keyframes z5h_timeline__flash__### {
    from, 50%, to { opacity: 1 }
    25%, 75%      { opacity: 0 }
}

/*=== crossfade ===*/
@keyframes z5h_timeline__crossfade__### {
    from { opacity: 0;
    background-color: white
    }
    to   { opacity: 1;
    background-color: white
    }
}

"""
        |> (\s ->
                String.join "\n"
                    [ s |> String.replace "###" "0"
                    , s |> String.replace "###" "1"
                    ]
           )
