module Transitions exposing (..)

import Discrete.Status
import Discrete.Timeline
import Html exposing (Html)
import Html.Attributes
import Internal.Util exposing (..)
import Timeline as Timeline



-- We didn't have a value, now we do, or vice versa
-- Nothing -> Just a
-- We have a value, but we're hiding it, then showing


type Reveal
    = FadeIn
    | ScaleInX
    | ScaleInY
    | SlideInFromLeft
    | SlideInFromTop
    | SlideInFromRight
    | SlideInFromBottom


type Flourish
    = Pulse
    | Flash


type Crossfade
    = Crossfade


revealName : Reveal -> String
revealName reveal =
    "z5h_timeline__"
        ++ (case reveal of
                SlideInFromLeft ->
                    "slide_in_from_left"

                SlideInFromRight ->
                    "slide_in_from_right"

                FadeIn ->
                    "fade_in"

                ScaleInX ->
                    "scale_in_x"

                ScaleInY ->
                    "scale_in_y"

                _ ->
                    "xxx"
           )


flourishName : Flourish -> String
flourishName flourish =
    "z5h_timeline__"
        ++ (case flourish of
                Pulse ->
                    "pulse"

                Flash ->
                    "flash"
           )


crossfadeName : Crossfade -> String
crossfadeName crossfade =
    "z5h_timeline__"
        ++ (case crossfade of
                Crossfade ->
                    "crossfade"
           )


animatedName : Int -> String -> String
animatedName flipFlop name =
    name ++ "__" ++ String.fromInt flipFlop


unrevealedAttributes : Reveal -> List (Html.Attribute msg)
unrevealedAttributes reveal =
    [ classes [ revealName reveal, "start" ] ]


revealAttributes : { reveal : Reveal, revealing : Bool, progress : Float, duration : Int, flipFlop : Int } -> List (Html.Attribute msg)
revealAttributes { reveal, revealing, duration, progress, flipFlop } =
    let
        delay =
            "-" ++ (String.fromFloat <| (1.0 - progress) * toFloat duration) ++ "ms"

        name =
            revealName reveal
    in
    [ Html.Attributes.style "animation-delay" delay
    , Html.Attributes.style "animation-name" (name |> animatedName flipFlop)
    , Html.Attributes.style "animation-direction"
        (if revealing then
            "normal"

         else
            "reverse"
        )
    , Html.Attributes.style "animation-fill-mode" "forwards"
    , Html.Attributes.style "animation-duration" (String.fromInt duration ++ "ms")
    , Html.Attributes.style "animation-timing-function" niceBezierString
    , classes [ name, "in-progress" ]
    ]


flourishAttributes : { flourish : Flourish, progress : Float, duration : Int, flipFlop : Int } -> List (Html.Attribute msg)
flourishAttributes { flourish, duration, progress, flipFlop } =
    let
        delay =
            "-" ++ (String.fromFloat <| (1.0 - progress) * toFloat duration) ++ "ms"

        _ =
            Debug.log "delaying" delay

        name =
            flourishName flourish
    in
    [ Html.Attributes.style "animation-delay" delay
    , Html.Attributes.style "animation-name" (name |> animatedName flipFlop)
    , Html.Attributes.style "animation-direction" "forwards"
    , Html.Attributes.style "animation-fill-mode" "forwards"
    , Html.Attributes.style "animation-timing-function" niceBezierString
    , Html.Attributes.style "animation-duration" (String.fromInt duration ++ "ms")

    --, Html.Attributes.style "animation-timing-function" niceBezierString
    , classes [ name, "in-progress" ]
    ]


crossfadeAttributes : { progress : Float, duration : Int, flipFlop : Int } -> List (Html.Attribute msg)
crossfadeAttributes { duration, progress, flipFlop } =
    let
        delay =
            "-" ++ (String.fromFloat <| (1.0 - progress) * toFloat duration) ++ "ms"

        name =
            crossfadeName Crossfade
    in
    [ Html.Attributes.style "animation-delay" delay
    , Html.Attributes.style "animation-name" (name |> animatedName flipFlop)
    , Html.Attributes.style "animation-direction" "forwards"
    , Html.Attributes.style "animation-fill-mode" "forwards"
    , Html.Attributes.style "animation-timing-function" niceBezierString
    , Html.Attributes.style "animation-duration" (String.fromInt duration ++ "ms")

    --, Html.Attributes.style "animation-timing-function" niceBezierString
    , classes [ name, "in-progress" ]
    ]


revealNode :
    String
    -> { reveal : Reveal, duration : Int }
    -> Discrete.Timeline.Timeline (Maybe t)
    -> List (Html.Attribute msg)
    -> (Discrete.Timeline.Timeline t -> List (Html msg))
    -> Html msg
revealNode node { reveal, duration } timeline attributes childView =
    let
        revealedStatus =
            timeline
                |> Discrete.Timeline.map ((/=) Nothing)
                |> Discrete.Timeline.transition Discrete.Timeline.Reversible duration
                |> Discrete.Status.toTimelineStatus

        maybeContentTimeline =
            timeline |> Discrete.Timeline.definitely
    in
    case ( revealedStatus, maybeContentTimeline ) of
        ( Timeline.At False, _ ) ->
            Html.node node (unrevealedAttributes reveal ++ attributes) []

        ( Timeline.At True, Just contentTimeline ) ->
            Html.node node
                attributes
                (childView contentTimeline)

        ( Timeline.Transitioning from to f, Just contentTimeline ) ->
            let
                flipFlop =
                    timeline |> Discrete.Timeline.unwrap |> .flipFlop

                allAttributes =
                    revealAttributes { reveal = reveal, revealing = to, duration = duration, progress = f, flipFlop = flipFlop }
            in
            Html.node node
                (allAttributes ++ attributes)
                (childView contentTimeline)

        ( Timeline.Transitioning _ False _, Nothing ) ->
            -- can't happen
            Html.node node attributes []

        ( Timeline.Transitioning _ True _, Nothing ) ->
            -- can't happen
            Html.node node attributes []

        ( Timeline.At True, Nothing ) ->
            -- can't happen
            Html.node node attributes []


flourishNode :
    String
    -> { flourish : Flourish, duration : Int }
    -> Discrete.Timeline.Timeline t
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
flourishNode node { flourish, duration } timeline attributes childView =
    let
        status =
            timeline
                |> Discrete.Timeline.transition Discrete.Timeline.Restartable duration
                |> Discrete.Status.toTimelineStatus
    in
    case status of
        Timeline.At t ->
            Html.node node attributes childView

        Timeline.Transitioning from to f ->
            let
                flipFlop =
                    timeline |> Discrete.Timeline.unwrap |> .flipFlop

                allAttributes =
                    flourishAttributes { flourish = flourish, duration = duration, progress = f, flipFlop = flipFlop }
            in
            Html.node node
                (allAttributes ++ attributes)
                childView


crossfadeNode :
    String
    -> Int
    -> Discrete.Timeline.Timeline t
    -> List (Html.Attribute msg)
    -> (t -> List (Html msg))
    -> Html msg
crossfadeNode node duration timeline attributes childView =
    let
        statusList =
            timeline
                |> Discrete.Timeline.transitions duration
                |> List.map Discrete.Status.toTimelineStatus
    in
    statusList
        |> List.map
            (\status ->
                case status of
                    Timeline.At at ->
                        Html.div
                            [ Html.Attributes.style "position" "absolute"
                            , Html.Attributes.style "left" "0"
                            , Html.Attributes.style "top" "0"
                            ]
                            (childView at)

                    Timeline.Transitioning from to f ->
                        let
                            flipFlop =
                                timeline |> Discrete.Timeline.unwrap |> .flipFlop

                            allAttributes =
                                crossfadeAttributes { duration = duration, progress = f, flipFlop = flipFlop }
                        in
                        Html.div
                            (allAttributes
                                ++ [ Html.Attributes.style "position" "absolute"
                                   , Html.Attributes.style "left" "0"
                                   , Html.Attributes.style "top" "0"
                                   ]
                            )
                            (childView to)
            )
        |> Html.node node (attributes ++ [ Html.Attributes.style "position" "relative" ])


classes : List String -> Html.Attribute msg
classes list =
    list |> List.map (\l -> ( l, True )) |> Html.Attributes.classList



-- either there is something that changes and you want a repeatable flourish upon change
-- Flourish
-- or, something goes between Nothing and Just, in which case you want a nice way to appear/disappear
-- Appear
-- or a toggle between N (=2??) states.
-- Toggle
