module Timeline exposing
    ( Timeline, Msg, Status(..)
    , init, update, subscriptions, view, viewDocument, msg
    , value, transition, map, withDefault
    , currentTime
    , push, sequence
    )

{-|


# Introduction

Timeline helps you animate the state changes of your `Model`.

It is designed to be very simple to integrate (a few minutes) and simultaneously
provides direct access to your model as well as a `Timeline Model` for animation
purposes.

**Important:** Timeline keeps a (garbage collected) history of your model states.
If your model updates at a modest pace then this API should be fine.

> When a pause between model changes exceeds the transition/animation duration,
> then model history can be discarded.

If your model has very high churn (e.g. You're doing `Time.every 5`) then this is
not the API for you.


# Basic Setup

Integrating is simple. Once you understand the principles,
it should take only a few minutes of work.

Assuming your app's `main` looks something like:

    main : Program () Model Msg
    main =
        Browser.application
            { init = init
            , update = update
            , view = view
            , subscriptions = subscriptions
            , onUrlChange = OnUrlChange
            , onUrlRequest = OnUrlRequest
            }

it needs to be changed to:

    main : Program () (Timeline Model) (Timeline.Msg Msg)
    main =
        Browser.application
            { init =
                \flags url navigationKey ->
                    init flags url navigationKey
                        |> Timeline.init 2000 (Time.millisToPosix 0)
            , update = Timeline.update update
            , view = Timeline.viewDocument view
            , subscriptions = Timeline.subscriptions subscriptions
            , onUrlChange = Timeline.msg << OnUrlChange
            , onUrlRequest = Timeline.msg << OnUrlRequest
            }

In short, we are using functions in timeline to turn our program into a

    Program () (Timeline Model) (Timeline.Msg Msg)

Note: The first two parameters to `Timeline.init` are the max duration of an animation (this is to aid garbage collection),
and the current time (safe to set to `0` for now, and discuss later).

Your `init`, `update` and `subscriptions` remain unchanged.

Your `view`, instead of having type

    view : Model -> Html Msg

will now have type

    view : Timeline Model -> Html Msg

A simple change to your view function should allow your program to run as it did:

    view : Timeline Model -> Html Msg
    view timeline =
        let
            model =
                Timeline.value timeline
        in
        ... -- your code as it used to work using model

Nothing exciting will happen yet, but now you have a `Timeline` at your disposal,
and this can be used to generate animations.


# Animating

As seen, given a `timeline` of your model, you can get the current value via:

    Timeline.value timeline

You can also ask something like:

     Timeline.transition 500 timeline

this says: suppose transitions between different states take 500ms. Where am I at?
The answer is of type `Status` which is either:

1.  `At t int`: there is no current transition, we have been at this value for `int` milliseconds
2.  `Transitioning t t float`: we are transitioning from an old `t` to a new one, and float goes from `1.0 -> 0.0`
    as we approach

Now that you know if you are statically at a state, or transitioning to that
state from a previous one, you can use the transition factor to do animations.


# Important types

@docs Timeline, Msg, Status


# Integration helpers

@docs init, update, subscriptions, view, viewDocument, msg


# Rendering and animating during view

@docs value, transition, map, withDefault


# Utility

@docs currentTime

-}

import Browser
import Browser.Events
import Diff
import Html exposing (Html)
import Internal.Util exposing (..)
import Time exposing (Posix)



-- Stuff
--


type alias Event t =
    { time : Posix, value : t }


{-| Map the value part of an event.
-}
mapEvent : (t -> s) -> Event t -> Event s
mapEvent f e =
    Event e.time (f e.value)


{-| A timeline of your model's history.
History is only held long enough to ensure smooth transitions,
based on the maximum transition duration value supplied to `init`.
You never need to create these directly.
-}
type alias Timeline t =
    { current : Event t
    , history : List (Event t)
    , limit : Int
    , now : Posix
    , start : Posix
    }


{-| Get the current time from a timeline.
Updated internally via `Browser.Events.onAnimationFrame`, thus inheriting its
resolution.
-}
currentTime : Timeline t -> Posix
currentTime =
    .now


{-| A timeline Msg.
-}
type Msg m
    = Msg m
    | UpdateTime Posix


{-| Converts the output of your existing `init` to a Timeline compatible `init`.

    limit =
        500

    now =
        Time.millisToPosix 0

    timelineInit =
        \flags -> myInit flags |> Timeline.init limit now

The `limit` parameter is the duration your longest animation (transition) will take.
This helps Timeline know when to throw away history it no longer needs.
When you do `timeline |> Timeline.transition 500` you are creating a 500ms transition.
If that's the longest transition in your app, then `limit` in `init` should be `500`.

The `now` parameter will become the timestamp for the initial model state.
It's perfectly safe to set this to any time in history (e.g. `Time.millisToPosix 0`).
The only caveat is if you later inquire how long a state has been at rest, you won't
get a correct answer for your initial state. This probably doesn't matter in
most use cases. But seeing as you can animate a static state based on it's age,
this is important to know.

-}
init : Int -> Posix -> ( model, Cmd msg ) -> ( Timeline model, Cmd (Msg msg) )
init limit now ( model, cmd ) =
    ( Timeline { value = model, time = now } [] limit now now
    , Cmd.map Msg cmd
    )


{-| Converts your existing `update` to a Timeline compatible `update`.

See Basic Setup.

-}
update : (msg -> model -> ( model, Cmd msg )) -> (Msg msg -> Timeline model -> ( Timeline model, Cmd (Msg msg) ))
update update_ msg_ timeline =
    case msg_ of
        UpdateTime posix ->
            ( { timeline | now = posix }, Cmd.none )

        Msg msg__ ->
            let
                ( newModel, cmd ) =
                    update_ msg__ (value timeline)
            in
            ( timeline |> push { value = newModel, time = timeline.now }
            , Cmd.map Msg cmd
            )


{-| Converts your existing `subscriptions` to a Timeline compatible `subscriptions`.

See Basic Setup.

-}
subscriptions : (model -> Sub msg) -> Timeline model -> Sub (Msg msg)
subscriptions subscriptions_ timeline =
    Sub.batch
        [ Browser.Events.onAnimationFrame UpdateTime
        , timeline |> value |> subscriptions_ |> Sub.map Msg
        ]


{-| Convenience function.

Converts your `view : Timeline model -> Html msg` to
a `Timeline model -> Html (Timeline.Msg msg)` for use in your `main`.

-}
view : (Timeline model -> Html msg) -> (Timeline model -> Html (Msg msg))
view view_ =
    view_ >> Html.map Msg


{-| Convenience function.

Converts your `view : Timeline model -> Browser.Document msg` to
a `Timeline model -> Browser.Document (Timeline.Msg msg)` for use in your
`main`.

-}
viewDocument : (Timeline model -> Browser.Document msg) -> (Timeline model -> Browser.Document (Msg msg))
viewDocument view_ =
    view_
        >> (\document ->
                { title = document.title
                , body = document.body |> List.map (Html.map Msg)
                }
           )


{-| Maps a `msg` to a `Timeline.Msg msg`.

Useful in your `main` when defining things such as:

    onUrlChange =
        Timeline.msg << OnUrlChange

-}
msg : m -> Msg m
msg =
    Msg


push : Event t -> Timeline t -> Timeline t
push e timeline =
    let
        now =
            e.time
    in
    if e.value == timeline.current.value then
        { timeline
            | now = now
        }

    else
        let
            garbageCollectHistory =
                timeDiff timeline.current.time timeline.now > timeline.limit
        in
        { timeline
            | current = e
            , history =
                timeline.current
                    -- if the start of history is so old that it's stable, we don't need older history
                    :: (if garbageCollectHistory then
                            []

                        else
                            timeline.history
                       )
            , now = now
            , start =
                if garbageCollectHistory then
                    timeline.current.time

                else
                    timeline.start
        }


{-| Extract the current (most recent) value from the timeline.
-}
value : Timeline t -> t
value =
    .current >> .value


{-| Maps a timeline. **Very importantly** the new timeline can have a different change history.

If

    type alias Model =
        { a : Int, b : Int }

then

    modelTimeline |> Timeline.map .a

returns a timeline of the history of `a` irrespective of `b`.

    modelTimeline |> Timeline.map .b

returns a timeline of the history of `b` irrespective of `a`.

One can thus animate transitions of `a` and `b` (or any other model properties)
independently.

-}
map : (t -> s) -> Timeline t -> Timeline s
map f { current, history, now, limit } =
    let
        ( first, rest ) =
            reverseNonEmpty ( current, history )

        mapf =
            mapEvent f

        initial =
            { current = mapf first, history = [], now = first.time, start = first.time, limit = limit }
    in
    List.foldl
        (\e t -> push (mapf e) t)
        initial
        rest
        |> (\timeline -> { timeline | now = now })


{-| This is for treating non-continuous Timelines as continuous. Usually occurs
when mapping your model (and hence Timeline) can result in unwanted maybes.

e.g.

    type alias PageAModel =
        { a : Bool }

    type alias PageBModel =
        { b : Bool }

    type Model
        = PageA PageAModel
        | PageB PageBModel

Here, the state of the page models are not continuous. They don't always exist.
Elm's type system will remind us that we cannot animate a thing that might not exist.

To animate `PageA` and `PageB` views, the following is required:

1.  have functions to extract `Maybe Page?Model` values from `Model`
2.  create a timeline of Maybe values and use withDefault

e.g.

    pageAModel : Model -> Maybe PageAModel
    pageAModel model =
        case model of
            PageA pageAModel_ ->
                Just pageAModel_

            _ ->
                Nothing


    pageBModel : Model -> Maybe PageBModel
    pageBModel model =
        ...

    view : Timeline Model -> Html Msg
    view timeline =
        let
            model =
                Timeline.value timeline
        in
        case model of
            PageA currentPageAModel ->
                let
                    continuousPageATimeline : Timeline PageAModel
                    continuousPageATimeline =
                        timeline
                            |> Timeline.map pageAModel
                            |> Timeline.withDefault currentPageAModel
                in
                    renderPageA continuousPageATimeline

            PageB currentPageBModel ->
                ...

-}
withDefault : t -> Timeline (Maybe t) -> Timeline t
withDefault t timeline =
    let
        current =
            case timeline.current.value of
                Just value_ ->
                    { value = value_, time = timeline.current.time }

                Nothing ->
                    { value = t, time = timeline.current.time }

        maybeEvent event =
            case event.value of
                Just value_ ->
                    Just { value = value_, time = event.time }

                Nothing ->
                    Nothing
    in
    { current = current
    , history = timeline.history |> List.filterMap maybeEvent
    , now = timeline.now
    , limit = timeline.limit
    , start = timeline.start
    }


{-| A transition status. Either `At` a value for a specific number of milliseconds,
or `Transitioning` from one value to another, with a "remaining" float value.

Note that during a transition, the float value goes from `1.0 -> 0.0`.
The "remaining" float value returned is not linear, but a dampened-spring
eased value. Use it directly (with a multiplier) to fade, scale, translate
with a natural and pleasant effect.

-}
type Status t
    = At t Int
    | Transitioning t t Float


{-| Given a duration for how long a transition on a timeline takes,
what's the status of our timeline?
Are we transitioning between values, or statically at a value?

    let
    hamburgerMenuStatus =
        modelTimeline
        |> Timeline.map .hamburgerMenuState
        |> transition 300
    in
        case hamburgerMenuState of
            At state t ->
                -- our menu is fully open/closed, and has been for `t` ms

            Transitioning from to remaining ->
                -- our menu is transitioning.
                -- `remaining` goes `1.0 -> 0.0` as `from -> to`


    The `remaining` float value returned is not linear, but a dampened-spring
    eased value. Use it directly (with a multiplier) to fade, scale, translate
    with a natural and pleasant effect.

-}
transition : Int -> Timeline t -> Status t
transition duration timeline =
    transitionHelper timeline.now duration timeline.current timeline.history


{-|

    Turns a `Timeline (List a)` to a `List (Timeline (Maybe a))`.

    This is useful, because when look at a single value (a String) it's
    sufficient to to know its history to animate accordingly.

    When we look at a changing list, typically that list is changing because
    values are being inserted or removed or modified.
    So, if we are rendering a list into a list-like view, we want to know
    what the Timeline is for each slot. That's what this is for.

    Because a slot (in this visualisation) might start as empty and have
    something inserted, or vice versa, we return a `Timeline` of `Maybe a`.

    The initial parameter is an `id` function which allows us to track an
    entry's index in the list as it's value changes over time.

-}
sequence : (a -> id) -> Timeline (List a) -> List (Timeline (Maybe a))
sequence id timeline =
    let
        ( head, tail ) =
            reverseNonEmpty ( timeline.current, timeline.history )

        initialTimelines : List (Timeline (Maybe a))
        initialTimelines =
            head.value
                |> List.map
                    (\singListValue ->
                        Timeline (Event head.time (Just <| singListValue)) [] timeline.limit head.time head.time
                    )

        zip : Posix -> List (Diff.Change (Maybe a)) -> List (Timeline (Maybe a)) -> List (Timeline (Maybe a))
        zip now diffs timelines =
            case ( diffs, timelines ) of
                ( [], [] ) ->
                    []

                ( (Diff.Added (Just a)) :: rest, _ ) ->
                    (Timeline (Event timeline.start Nothing) [] timeline.limit timeline.start timeline.start
                        |> push (Event now (Just a))
                    )
                        :: zip now rest timelines

                ( (Diff.Removed (Just _)) :: rest, timelinesH :: timelinesT ) ->
                    (timelinesH |> push (Event now Nothing))
                        :: zip now rest timelinesT

                ( (Diff.Removed Nothing) :: rest, timelinesH :: timelinesT ) ->
                    timelinesH
                        :: zip now rest timelinesT

                ( (Diff.NoChange (Just a)) :: rest, timelinesH :: timelinesT ) ->
                    (timelinesH |> push (Event now (Just a)))
                        :: zip now rest timelinesT

                -- impossible:
                -- we never put Nothing into a state we're transitioning into
                ( (Diff.Added Nothing) :: _, _ ) ->
                    timelines

                ( (Diff.NoChange Nothing) :: _, _ ) ->
                    timelines

                -- impossible: we should have an entry for every value in the
                -- timeline
                ( [], _ :: _ ) ->
                    timelines

                -- impossible:
                -- if we removed or no-changed a timeline, we should have the
                -- timeline in hand
                ( (Diff.Removed Nothing) :: _, [] ) ->
                    timelines

                ( (Diff.Removed (Just _)) :: _, [] ) ->
                    timelines

                ( (Diff.NoChange (Just _)) :: _, [] ) ->
                    timelines

        step : Event (List a) -> List (Timeline (Maybe a)) -> List (Timeline (Maybe a))
        step event timeLines =
            let
                lastValues : List (Maybe a)
                lastValues =
                    timeLines
                        |> List.map
                            (\timeline_ ->
                                timeline_.current.value
                            )

                currentValues : List (Maybe a)
                currentValues =
                    event.value |> List.map Just

                diff : List (Diff.Change (Maybe a))
                diff =
                    Diff.diff
                        (lastValues |> List.map (Maybe.map id))
                        (currentValues |> List.map (Maybe.map id))
                        |> List.map
                            (\maybeDiffedId ->
                                case maybeDiffedId of
                                    Diff.Added Nothing ->
                                        -- not possible
                                        Nothing

                                    Diff.Added (Just i) ->
                                        listFind (\t -> Maybe.map id t == Just i) currentValues
                                            |> Maybe.map (\t -> Diff.Added t)

                                    Diff.Removed Nothing ->
                                        Just (Diff.Removed Nothing)

                                    Diff.Removed (Just i) ->
                                        listFind (\t -> Maybe.map id t == Just i) lastValues
                                            |> Maybe.map (\t -> Diff.Removed t)

                                    Diff.NoChange Nothing ->
                                        -- not possible
                                        Nothing

                                    Diff.NoChange (Just i) ->
                                        listFind (\t -> Maybe.map id t == Just i) currentValues
                                            |> Maybe.map (\t -> Diff.NoChange t)
                            )
                        |> List.filterMap identity
            in
            zip event.time diff timeLines
    in
    List.foldl step initialTimelines tail


transitionHelper : Posix -> Int -> Event t -> List (Event t) -> Status t
transitionHelper now duration e events =
    case events of
        [] ->
            At e.value (timeDiff now e.time)

        prev :: [] ->
            spring 1.0 (timeDiff now e.time) duration
                |> (\springPos ->
                        if springPos == 0 then
                            At e.value (timeDiff now e.time)

                        else
                            Transitioning prev.value e.value springPos
                   )

        prev :: rest ->
            let
                midTransition =
                    transitionHelper now duration prev rest
            in
            case midTransition of
                At prevValue _ ->
                    spring 1.0 (timeDiff now e.time) duration
                        |> (\springPos ->
                                if springPos == 0 then
                                    At e.value (timeDiff now e.time)

                                else
                                    Transitioning prevValue e.value springPos
                           )

                Transitioning _ prevValue transitionSpringPos ->
                    spring (1 - transitionSpringPos)
                        (timeDiff now e.time)
                        duration
                        |> (\springPos ->
                                if springPos == 0 then
                                    At e.value (timeDiff now e.time)

                                else
                                    Transitioning prevValue e.value springPos
                           )


spring : Float -> Int -> Int -> Float
spring x0 complete duration =
    let
        dt =
            toFloat complete / toFloat duration
    in
    if dt > 1.0 then
        0.0

    else
        (x0 + 8 * x0 * dt) * (e ^ (-8 * dt))
