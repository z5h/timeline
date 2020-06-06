module Discrete.Transitions exposing
    ( Flourish(..)
    , Reveal(..)
    , Toggle(..)
    , crossfadeNode
    , flourishNode
    , revealNode
    , toggleNode
    )

{-|

    @docs revealNode, Reveal

    @docs flourishNode, Flourish

    @docs crossfadeNode, Crossfade

-}

import Discrete.Status
import Discrete.Timeline
import Html exposing (Html)
import Html.Attributes
import Internal.Util exposing (..)
import Timeline as Timeline


{-| For Timelines who's values alternate between `Nothing` and `Just` values.

Used to animate something appearing and disappearing form view. Accordingly,
the animation fades and scales to/from nothing.

-}
type Reveal
    = ScaleIn
    | ScaleInX
    | ScaleInY


{-| Animate a toggle from a `Timeline Bool`.

SlideInFromXXX toggles are useful for animating a slide-in menu.

RotateXXX are useful for animating

-}
type Toggle
    = SlideInFromLeft
    | SlideInFromTop
    | SlideInFromRight
    | SlideInFromBottom
    | Rotate90
    | Rotate180
    | Rotate360
    | RotateNeg90
    | RotateNeg180
    | RotateNeg360


{-| Animate a Flourish anytime a timeline value changes.
-}
type Flourish
    = Pulse
    | Flash


revealName : Reveal -> String
revealName reveal =
    "z5h_timeline__"
        ++ (case reveal of
                ScaleIn ->
                    "fade_in"

                ScaleInX ->
                    "scale_in_x"

                ScaleInY ->
                    "scale_in_y"
           )


toggleName : Toggle -> String
toggleName entrance =
    "z5h_timeline__"
        ++ (case entrance of
                SlideInFromLeft ->
                    "slide_in_from_left"

                SlideInFromTop ->
                    "slide_in_from_top"

                SlideInFromRight ->
                    "slide_in_from_right"

                SlideInFromBottom ->
                    "slide_in_from_bottom"

                Rotate90 ->
                    "rotate_90"

                Rotate180 ->
                    "rotate_180"

                Rotate360 ->
                    "rotate_360"

                RotateNeg90 ->
                    "rotate_neg90"

                RotateNeg180 ->
                    "rotate_neg180"

                RotateNeg360 ->
                    "rotate_neg360"
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


animatedName : Int -> String -> String
animatedName flipFlop name =
    name ++ "__" ++ String.fromInt flipFlop


unrevealedAttributes : Reveal -> List (Html.Attribute msg)
unrevealedAttributes reveal =
    [ classes [ revealName reveal, "start" ] ]


toggleEndAttributes : Toggle -> List (Html.Attribute msg)
toggleEndAttributes toggle =
    [ classes [ toggleName toggle, "end" ] ]


toggleStartAttributes : Toggle -> List (Html.Attribute msg)
toggleStartAttributes toggle =
    [ classes [ toggleName toggle, "start" ] ]


revealAttributes : { reveal : Reveal, forward : Bool, progress : Float, duration : Int, flipFlop : Int } -> List (Html.Attribute msg)
revealAttributes { reveal, forward, duration, progress, flipFlop } =
    let
        delay =
            "-" ++ (String.fromFloat <| (1.0 - progress) * toFloat duration) ++ "ms"

        name =
            revealName reveal
    in
    [ Html.Attributes.style "animation-delay" delay
    , Html.Attributes.style "animation-name" (name |> animatedName flipFlop)
    , Html.Attributes.style "animation-direction"
        (if forward then
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


toggleAttributes : { toggle : Toggle, forward : Bool, progress : Float, duration : Int, flipFlop : Int } -> List (Html.Attribute msg)
toggleAttributes { toggle, duration, forward, progress, flipFlop } =
    let
        delay =
            "-" ++ (String.fromFloat <| (1.0 - progress) * toFloat duration) ++ "ms"

        name =
            toggleName toggle
    in
    [ Html.Attributes.style "animation-delay" delay
    , Html.Attributes.style "animation-name" (name |> animatedName flipFlop)
    , Html.Attributes.style "animation-direction"
        (if forward then
            "normal"

         else
            "reverse"
        )
    , Html.Attributes.style "animation-fill-mode" "forwards"
    , Html.Attributes.style "animation-duration" (String.fromInt duration ++ "ms")
    , Html.Attributes.style "animation-timing-function" niceBezierString
    , classes [ name, "in-progress" ]
    ]


crossfadeAttributes : { progress : Float, duration : Int, flipFlop : Int, forward : Bool } -> List (Html.Attribute msg)
crossfadeAttributes { duration, progress, flipFlop, forward } =
    let
        delay =
            "-" ++ (String.fromFloat <| (1.0 - progress) * toFloat duration) ++ "ms"

        name =
            "z5h_timeline__crossfade"
    in
    [ Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "animation-delay" delay
    , Html.Attributes.style "animation-name" (name |> animatedName flipFlop)
    , Html.Attributes.style "animation-direction"
        (if forward then
            "forwards"

         else
            "reverse"
        )
    , Html.Attributes.style "animation-fill-mode" "forwards"
    , Html.Attributes.style "animation-timing-function"
        (if forward then
            "ease-out"

         else
            "ease-in"
        )
    , Html.Attributes.style "animation-duration" (String.fromInt duration ++ "ms")

    --, Html.Attributes.style "animation-timing-function" niceBezierString
    , classes [ name, "in-progress" ]
    ]


toggleNode :
    String
    -> { toggle : Toggle, duration : Int }
    -> Discrete.Timeline.Timeline Bool
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
toggleNode node { toggle, duration } timeline attributes childView =
    let
        status =
            timeline
                |> Discrete.Timeline.transition duration { interrupt = Discrete.Timeline.Reverse }
                |> Discrete.Status.toTimelineStatus
    in
    case status of
        Timeline.At True ->
            Html.node node (toggleStartAttributes toggle ++ attributes) childView

        Timeline.At False ->
            Html.node node (toggleEndAttributes toggle ++ attributes) childView

        Timeline.Transitioning _ forward f ->
            let
                flipFlop =
                    timeline |> Discrete.Timeline.unwrap |> .flipFlop

                allAttributes =
                    toggleAttributes { toggle = toggle, forward = forward, duration = duration, progress = f, flipFlop = flipFlop }
            in
            Html.node node
                (allAttributes ++ attributes)
                childView


{-| Render a node which will animate reveal transitions
-}
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
                |> Discrete.Timeline.transition duration { interrupt = Discrete.Timeline.Reverse }
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
                    revealAttributes { reveal = reveal, forward = to, duration = duration, progress = f, flipFlop = flipFlop }
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


{-| Render a node which will animate flourish transitions
-}
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
                |> Discrete.Timeline.transition duration { interrupt = Discrete.Timeline.Peak }
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


{-| Render a node which will animate crossfade transitions
-}
crossfadeNode :
    String
    -> Int
    -> Discrete.Timeline.Timeline t
    -> List (Html.Attribute msg)
    -> (t -> List (Html msg))
    -> Html msg
crossfadeNode node duration timeline attributes childView =
    let
        ( topStatus, remainingStatuses ) =
            timeline
                |> Discrete.Timeline.transitions duration
                |> (\( top, remaining ) ->
                        ( Discrete.Status.toTimelineStatus top
                        , List.map Discrete.Status.toTimelineStatus remaining
                        )
                   )

        flipFlop =
            timeline |> Discrete.Timeline.unwrap |> .flipFlop

        transitionView : Maybe t -> Maybe t -> Float -> Maybe (Html msg)
        transitionView maybeFrom maybeTo f =
            case ( maybeFrom, maybeTo ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( _, Just to ) ->
                    Just
                        (Html.div
                            (crossfadeAttributes { duration = duration, progress = f, flipFlop = flipFlop, forward = True })
                            (childView to)
                        )

                ( Just from, Nothing ) ->
                    Just
                        (Html.div
                            (crossfadeAttributes { duration = duration, progress = f, flipFlop = flipFlop, forward = False })
                            (childView from)
                        )
    in
    Html.node node
        (attributes ++ [ Html.Attributes.style "position" "relative" ])
        ((case topStatus of
            Timeline.At (Just at) ->
                [ Html.div [ Html.Attributes.style "position" "absolute" ]
                    (childView at)
                ]

            Timeline.Transitioning _ (Just to) f ->
                transitionView Nothing (Just to) f
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []

            _ ->
                []
         )
            ++ (remainingStatuses
                    |> List.filterMap
                        (\remainingStatus ->
                            case remainingStatus of
                                Timeline.Transitioning from to f ->
                                    transitionView from to f

                                Timeline.At _ ->
                                    Nothing
                        )
               )
        )


classes : List String -> Html.Attribute msg
classes list =
    list |> List.map (\l -> ( l, True )) |> Html.Attributes.classList
