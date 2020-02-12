module TimelineTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Time exposing (Posix)
import Timeline exposing (Timeline)


type alias Event t =
    { time : Posix, value : t }


historyList : Timeline t -> List ( Int, t )
historyList timeline =
    timeline.current
        :: timeline.history
        |> List.reverse
        |> List.map (\e -> ( e.time |> Time.posixToMillis, e.value ))


new : t -> Timeline t
new t =
    let
        now =
            Time.millisToPosix 0
    in
    Timeline (Event now t) [] 1 now now


push : t -> Timeline t -> Timeline t
push t timeline =
    let
        now =
            timeline.now |> Time.posixToMillis |> (+) 1 |> Time.millisToPosix
    in
    timeline |> Timeline.push (Event now t)


tick : Timeline t -> Timeline t
tick timeline =
    { timeline | now = timeline.now |> Time.posixToMillis |> (+) 1 |> Time.millisToPosix }


suite : Test
suite =
    describe "Timeline"
        [ describe "create/update"
            [ test "new 1, value" <|
                \_ ->
                    let
                        timeline =
                            new 5
                    in
                    timeline
                        |> Timeline.value
                        |> Expect.equal 5
            , test "new 1, transition" <|
                \_ ->
                    let
                        timeline =
                            new 5
                    in
                    timeline
                        |> Timeline.transition 1
                        |> Expect.equal (Timeline.At 5 0)
            , test "new 1, push 2, value" <|
                \_ ->
                    let
                        timeline =
                            new 5 |> push 6
                    in
                    timeline
                        |> Timeline.value
                        |> Expect.equal 6
            , test "new 1, push 2, transition" <|
                \_ ->
                    let
                        timeline =
                            new 5 |> push 6
                    in
                    timeline
                        |> Timeline.transition 1
                        |> Expect.equal (Timeline.Transitioning 5 6 1)
            , test "gc" <|
                \_ ->
                    let
                        timeline =
                            new 5
                                |> push 6
                                |> push 7
                                |> tick
                                |> tick
                                |> push 8

                        -- we should drop everything before the 7
                    in
                    timeline |> historyList |> Expect.equal [ ( 2, 7 ), ( 5, 8 ) ]
            ]
        , describe "sequence"
            [ test "sequence base" <|
                \_ ->
                    let
                        timeline =
                            new [ 1, 2 ]

                        -- we should drop everything before the 7
                    in
                    timeline
                        |> Timeline.sequence identity
                        |> List.map historyList
                        |> Expect.equal [ [ ( 0, Just 1 ) ], [ ( 0, Just 2 ) ] ]
            , test "sequence remove last" <|
                \_ ->
                    let
                        timeline =
                            new [ 1, 2 ] |> push [ 1 ]

                        -- we should drop everything before the 7
                    in
                    timeline
                        |> Timeline.sequence identity
                        |> List.map historyList
                        |> Expect.equal [ [ ( 0, Just 1 ) ], [ ( 0, Just 2 ), ( 1, Nothing ) ] ]
            , test "sequence remove first" <|
                \_ ->
                    let
                        timeline =
                            new [ 1, 2 ] |> push [ 2 ]

                        -- we should drop everything before the 7
                    in
                    timeline
                        |> Timeline.sequence identity
                        |> List.map historyList
                        |> Expect.equal [ [ ( 0, Just 1 ), ( 1, Nothing ) ], [ ( 0, Just 2 ) ] ]
            , test "sequence remove all at once" <|
                \_ ->
                    let
                        timeline =
                            new [ 1, 2 ] |> push []

                        -- we should drop everything before the 7
                    in
                    timeline
                        |> Timeline.sequence identity
                        |> List.map historyList
                        |> Expect.equal
                            [ [ ( 0, Just 1 ), ( 1, Nothing ) ]
                            , [ ( 0, Just 2 ), ( 1, Nothing ) ]
                            ]
            , test "sequence remove in steps" <|
                \_ ->
                    let
                        timeline =
                            new [ 1, 2 ] |> push [ 1 ] |> push []

                        -- we should drop everything before the 7
                    in
                    timeline
                        |> Timeline.sequence identity
                        |> List.map historyList
                        |> Expect.equal
                            [ [ ( 0, Just 1 ), ( 2, Nothing ) ]
                            , [ ( 0, Just 2 ), ( 1, Nothing ) ]
                            ]
            , test "sequence add all" <|
                \_ ->
                    let
                        timeline =
                            new [] |> push [ 1, 2 ]

                        -- we should drop everything before the 7
                    in
                    timeline
                        |> Timeline.sequence identity
                        |> List.map historyList
                        |> Expect.equal
                            [ [ ( 0, Nothing ), ( 1, Just 1 ) ]
                            , [ ( 0, Nothing ), ( 1, Just 2 ) ]
                            ]
            , test "sequence add/remove stages" <|
                \_ ->
                    let
                        timeline =
                            new []
                                |> (\t -> { t | limit = 10 })
                                |> push [ 2 ]
                                |> push [ 1, 2 ]
                                |> push [ 1, 2, 3 ]
                                |> push [ 2, 3 ]
                                |> push [ 3 ]
                                |> push []

                        -- we should drop everything before the 7
                    in
                    timeline
                        |> Timeline.sequence identity
                        |> List.map historyList
                        |> Expect.equal
                            [ [ ( 0, Nothing ), ( 2, Just 1 ), ( 4, Nothing ) ]
                            , [ ( 0, Nothing ), ( 1, Just 2 ), ( 5, Nothing ) ]
                            , [ ( 0, Nothing ), ( 3, Just 3 ), ( 6, Nothing ) ]
                            ]
            , test "sequence add/remove stages track id" <|
                \_ ->
                    let
                        timeline =
                            new []
                                |> (\t -> { t | limit = 10 })
                                |> push [ { id = 2, val = "2a" } ]
                                |> push [ { id = 2, val = "2b" } ]
                                |> push [ { id = 1, val = "1a" }, { id = 2, val = "2c" }, { id = 3, val = "3a" } ]
                                |> push [ { id = 1, val = "1b" }, { id = 2, val = "2d" } ]
                                |> push []

                        -- we should drop everything before the 7
                    in
                    timeline
                        |> Timeline.sequence .id
                        |> List.map historyList
                        |> Expect.equal
                            [ [ ( 0, Nothing ), ( 3, Just { id = 1, val = "1a" } ), ( 4, Just { id = 1, val = "1b" } ), ( 5, Nothing ) ]
                            , [ ( 0, Nothing ), ( 1, Just { id = 2, val = "2a" } ), ( 2, Just { id = 2, val = "2b" } ), ( 3, Just { id = 2, val = "2c" } ), ( 4, Just { id = 2, val = "2d" } ), ( 5, Nothing ) ]
                            , [ ( 0, Nothing ), ( 3, Just { id = 3, val = "3a" } ), ( 4, Nothing ) ]
                            ]
            ]
        ]
