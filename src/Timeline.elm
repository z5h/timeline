module Timeline exposing
    ( Timeline, Msg, Status(..)
    , init, update, subscriptions, view, viewDocument, msg
    , value, map, Interrupt(..), transition, transitions, withDefault, sequence, definitely, ease
    , currentTime, bezier
    , push
    )

{-|


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


# Setup and integration helpers

@docs init, update, subscriptions, view, viewDocument, msg


# Rendering and animating during view

@docs value, map, Interrupt, transition, transitions, withDefault, sequence, definitely, ease


# Utility

@docs currentTime, bezier


# Other

@docs push

-}

import Browser
import Browser.Events
import Diff
import Html exposing (Html)
import Html.Lazy
import Internal.Util exposing (..)
import Time exposing (Posix)


{-| A Timeline Event is a timestamped model value.
-}
type alias Event m =
    { time : Posix, value : m }


{-| Map the value part of an event.
-}
mapEvent : (a -> b) -> Event a -> Event b
mapEvent f e =
    Event e.time (f e.value)


type alias Bit =
    Int


flip : Bit -> Bit
flip bit =
    if bit == 1 then
        0

    else
        1


{-| A timeline of the history of your model.
Given a timeline, and a value to animate, we can generate a continuous
transition.

Note that history is only held long enough to ensure smooth transitions,
based on the maximum transition duration value supplied to `init`. Older
values stored in timeline will be automatically discarded when not needed.
You never need to create these directly.

-}
type alias Timeline t =
    { current : Event t
    , history : List (Event t)
    , limit : Int
    , now : Posix
    , key : Posix
    , flipFlop : Bit
    }


longAgo : Posix
longAgo =
    0 |> Time.millisToPosix


{-| Get the current time from a timeline.
Updated internally via `Browser.Events.onAnimationFrame`, thus inheriting its
resolution.

Having this prevents the need from tracking a high resolution time value in your
model.

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

    timelineInit =
        \flags -> myInit flags |> Timeline.init limit

The `limit` parameter is the duration your longest animation (transition) will
take. This helps Timeline know when to throw away history it no longer needs.

-}
init : Int -> ( model, Cmd msg ) -> ( Timeline model, Cmd (Msg msg) )
init limit ( model, cmd ) =
    ( Timeline { value = model, time = longAgo } [] limit longAgo longAgo 0
    , Cmd.map Msg cmd
    )


{-| Converts your existing `update` to a Timeline compatible `update`.

See Basic Setup.

-}
update : (msg -> model -> ( model, Cmd msg )) -> (Msg msg -> Timeline model -> ( Timeline model, Cmd (Msg msg) ))
update update_ msg_ timeline =
    case msg_ of
        UpdateTime posix ->
            ( { timeline | now = posix } |> updateKey, Cmd.none )

        Msg msg__ ->
            let
                ( newModel, cmd ) =
                    update_ msg__ (value timeline)
            in
            ( timeline |> push { value = newModel, time = timeline.now } |> updateKey
            , Cmd.map Msg cmd
            )


updateKey : Timeline t -> Timeline t
updateKey timeline =
    if timeDiff timeline.now timeline.current.time > timeline.limit then
        let
            newKey =
                timeline.current.time |> addMilliseconds timeline.limit
        in
        if timeline.key == newKey then
            timeline

        else
            { timeline | key = newKey }

    else
        { timeline | key = timeline.now }


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

See Basic Setup.

-}
view : (Timeline model -> Html msg) -> (Timeline model -> Html (Msg msg))
view view_ timeline_ =
    Html.Lazy.lazy6
        lazyView
        timeline_.current
        timeline_.history
        timeline_.limit
        timeline_.key
        timeline_.flipFlop
        view_
        |> Html.map Msg


lazyView current history limit timestamp flipFlop view_ =
    view_ (Timeline current history limit timestamp timestamp flipFlop)


{-| Convenience function.

Converts your `view : Timeline model -> Browser.Document msg` to
a `Timeline model -> Browser.Document (Timeline.Msg msg)` for use in your
`main`.

See Basic Setup.

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


{-| Exposed for testing. You don't need this.
-}
push : Event t -> Timeline t -> Timeline t
push e timeline =
    let
        now =
            e.time

        flipFlop =
            flip timeline.flipFlop
    in
    if e.value == timeline.current.value then
        { timeline
            | now = now
            , flipFlop = flipFlop
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
            , flipFlop = flipFlop
        }


{-| Extract the current (most recent) value from the timeline.

At any point in your view code that doesn't need animations, simply retrieve the
current timeline value and proceed rendering the view as you previously would.

E.g. as a first step in integrating Timeline into your code, simply render your
existing view code:

    view : Timeline Model -> Html Msg
    view timeline =
        let
            model =
                Timeline.value timeline
        in
        ... -- your existing view code as it used to work using model

-}
value : Timeline t -> t
value =
    .current >> .value


{-| Maps a timeline. **Very importantly** the new timeline has it's own history.

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
map f { current, history, now, limit, flipFlop, key } =
    let
        ( first, rest ) =
            reverseNonEmpty ( current, history )

        mapf =
            mapEvent f

        initial =
            { current = mapf first
            , history = []
            , now = longAgo
            , key = longAgo
            , limit = limit
            , flipFlop = 0
            }
    in
    List.foldl
        (\e t -> push (mapf e) t)
        initial
        rest
        |> (\timeline -> { timeline | now = now, key = key, flipFlop = flipFlop })


{-| Extract the `Just` values from a Timeline of Maybes. In the case where
the input Timeline has no `Just` values, a `Nothing` is returned.
-}
definitely : Timeline (Maybe t) -> Maybe (Timeline t)
definitely { current, history, now, limit, flipFlop, key } =
    current
        :: history
        |> List.filterMap maybeEvent
        |> (\maybeEvents ->
                case maybeEvents of
                    [] ->
                        Nothing

                    first :: rest ->
                        let
                            initial =
                                { current = first
                                , history = []
                                , now = longAgo
                                , key = longAgo
                                , limit = limit
                                , flipFlop = 0
                                }
                        in
                        Just <|
                            (List.foldl
                                (\e t -> push e t)
                                initial
                                rest
                                |> (\timeline ->
                                        { timeline
                                            | now = now
                                            , key = key
                                            , flipFlop = flipFlop
                                        }
                                   )
                            )
           )


{-| This is for treating non-continuous Timelines as continuous.
Consider this typical example:

e.g.

    type alias PageAModel =
        { a : Bool }

    type alias PageBModel =
        { b : Bool }

    type Model
        = PageA PageAModel
        | PageB PageBModel

Here, the state of the page models are not continuous as viewed form the top
level. They don't always exist.
Elm's type system will remind us that we cannot animate a thing that might not
exist.

To animate `PageA` and `PageB` views, the following is required:

1.  functions that extract `Maybe PageAModel` and `Maybe PageBModel`
    values from `Model`
2.  create a timeline of `Maybe` values and use `withDefault`

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
        newFullHistory =
            (timeline.current :: timeline.history) |> List.filterMap maybeEvent

        ( newCurrent, newHistory ) =
            case newFullHistory of
                head :: tail ->
                    ( head, tail )

                [] ->
                    ( { value = t, time = timeline.current.time }, [] )
    in
    { current = newCurrent
    , history = newHistory
    , now = timeline.now
    , key = timeline.key
    , limit = timeline.limit
    , flipFlop = timeline.flipFlop
    }


maybeEvent : Event (Maybe b) -> Maybe (Event b)
maybeEvent event =
    case event.value of
        Just value_ ->
            Just { value = value_, time = event.time }

        Nothing ->
            Nothing


{-| A transition status.

Either `At` a value,
or `Transitioning` from one value to another, with a "remaining" float value.

Note that during a transition, the float value goes from `1.0 -> 0.0`.
Use that number (with a multiplier or easing function) to fade, scale,
translate, etc.

-}
type Status t
    = At t
    | Transitioning t t Float


{-| Applies an easing function to a Status.
An easing function should ensure 0 -> 0, 1 -> 1, and should be continuous.
-}
ease : (Float -> Float) -> Status t -> Status t
ease easingFunction status =
    case status of
        At t ->
            At t

        Transitioning t0 t1 f ->
            Transitioning t0 t1 (easingFunction f)


{-| Given a duration for how long a transition on a timeline takes,
what's the status of our timeline?
Are we transitioning between values, or statically at a value?

Any uninterrupted transition will be equivalent regardless of the interrupt
mode. Interrupted transitions will behave differently. See `Interrupt`.

Here we want to generate transitions to animate a menu. If a transition is
interrupted, we want to reverse progress.

    let
    menuStatus =
        modelTimeline
        |> Timeline.map .menuState
        |> transition 300 {interrupt = Reverse}
    in
        case menuStatus of
            At state ->
                -- our menu is fully open/closed, and has been for `t` ms

            Transitioning from to remaining ->
                -- our menu is transitioning.
                -- `remaining` goes `1.0 -> 0.0` as `from -> to`

-}
transition : Int -> { interrupt : Interrupt } -> Timeline t -> Status t
transition duration { interrupt } timeline =
    case interrupt of
        Reverse ->
            trackingTransitionHelper
                False
                timeline.now
                duration
                timeline.current
                timeline.history

        Restart ->
            interruptibleTransitionHelper
                timeline.now
                duration
                timeline.current
                timeline.history

        Peak ->
            trackingTransitionHelper
                True
                timeline.now
                duration
                timeline.current
                timeline.history


{-|

    A Timeline represents the (possibly interrupted) transitions of a value.
    E.g. `A` to `B` to `C`.
    The status of such a timeline's transition will either be
    `Transitioning B C f`, or `At C`.

    Another way of way of viewing this, is each value is it's own transition:

        Just A to Nothing
        Just B to Nothing
        Nothing to Just C

    That is, at some point we had an A, a B but they are transitioning away.
    And we have the appearance of a C.

    This function gives us that perspective. The result is a non-empty list.

-}
transitions : Int -> Timeline t -> ( Status (Maybe t), List (Status (Maybe t)) )
transitions duration timeline =
    case timeline.history of
        [] ->
            ( At <| Just timeline.current.value, [] )

        _ ->
            ( remaining (timeDiff timeline.now timeline.current.time) duration
                |> (\remaining_ ->
                        if remaining_ == 0 then
                            At <| Just timeline.current.value

                        else
                            Transitioning Nothing (Just timeline.current.value) remaining_
                   )
            , (timeline.current :: timeline.history)
                |> List.reverse
                |> listPairs
                |> List.filterMap
                    (\( event, followingEvent ) ->
                        let
                            -- fade in from event until following event created
                            fadeInRemaining =
                                remaining (timeDiff event.time followingEvent.time) duration

                            -- fade out once following event created until now
                            fadeOutRemainingFromFull =
                                remaining (timeDiff followingEvent.time timeline.now) duration

                            actualFadeOutRemaining =
                                fadeOutRemainingFromFull
                                    - fadeInRemaining
                        in
                        if actualFadeOutRemaining > 0 then
                            Just <| Transitioning (Just event.value) Nothing actualFadeOutRemaining

                        else
                            Nothing
                    )
                |> List.reverse
            )


{-| Interrupt.

Interrupt values tell transition generator how do deal with interrupted
transitions.

  - `Reverse`: this is useful when state changes are between 2 values, and
    animations have a reversible quality. (e.g. toggling an opening menu in progress).
    If the opening of a menu has value `Transitioning Closed Open 0.9`, then it has %90 of it's transition remaining.
    A model change from (Open to Closed) will, when transitioned with `Reverse`, have a value of
    `Transitioning Open Closed 0.1`.
  - `Restart`: this is for simply restarting an animation upon interruption.
  - `Peak`: this is useful for an animation that goes to a peak value and ends where it started.
    E.g. "Pulse" or "Flash" style animation. If interrupted, and the has not been reached, the transition will simply
    continue on. If interrupted and the peak has already been reached, the transition will reverse to "peak" again.

-}
type Interrupt
    = Reverse
    | Restart
    | Peak


{-| Turns a `Timeline (List a)` to a `List (Timeline (Maybe a))`.

When we look at a changing list, typically that list is changing because
values are being inserted or removed or modified.
So, if we are rendering a list into a list-like view, we want to know
what the Timeline is for **each slot**. That's what this is for.

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
                        Timeline (Event head.time (Just <| singListValue)) [] timeline.limit longAgo longAgo 0
                    )

        zip : Posix -> List (Diff.Change (Maybe a)) -> List (Timeline (Maybe a)) -> List (Timeline (Maybe a))
        zip now diffs timelines =
            case ( diffs, timelines ) of
                ( [], [] ) ->
                    []

                ( (Diff.Added (Just a)) :: rest, _ ) ->
                    (Timeline (Event longAgo Nothing) [] timeline.limit longAgo longAgo 0
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
        |> List.map (\t -> { t | now = timeline.now, flipFlop = timeline.flipFlop })


interruptibleTransitionHelper : Posix -> Int -> Event t -> List (Event t) -> Status t
interruptibleTransitionHelper now duration e events =
    case events of
        [] ->
            At e.value

        prev :: _ ->
            remaining (timeDiff now e.time) duration
                |> (\remaining_ ->
                        if remaining_ == 0 then
                            At e.value

                        else
                            Transitioning prev.value e.value remaining_
                   )


trackingTransitionHelper : Bool -> Posix -> Int -> Event t -> List (Event t) -> Status t
trackingTransitionHelper maximize now duration e events =
    case events of
        [] ->
            At e.value

        prev :: [] ->
            remaining (timeDiff now e.time) duration
                |> (\remaining_ ->
                        if remaining_ == 0 then
                            At e.value

                        else
                            Transitioning prev.value e.value remaining_
                   )

        prev :: rest ->
            let
                midTransition =
                    trackingTransitionHelper maximize e.time duration prev rest
            in
            case midTransition of
                At prevValue ->
                    remaining (timeDiff now e.time) duration
                        |> (\remaining_ ->
                                if remaining_ == 0 then
                                    At e.value

                                else
                                    Transitioning prevValue e.value remaining_
                           )

                Transitioning _ prevValue remaining_ ->
                    (if maximize then
                        if remaining_ <= 0.5 then
                            remaining (timeDiff now e.time) duration
                                - remaining_

                        else
                            remaining (timeDiff now e.time) duration
                                - (1 - remaining_)

                     else
                        remaining (timeDiff now e.time) duration
                            - remaining_
                    )
                        |> (\newRemaining_ ->
                                if newRemaining_ <= 0 then
                                    At e.value

                                else
                                    Transitioning prevValue e.value newRemaining_
                           )


remaining : Int -> Int -> Float
remaining complete duration =
    if complete >= duration then
        0.0

    else
        1.0 - toFloat complete / toFloat duration


{-| Generate a bezier easing function.

`(x0, y0)` and `(x3, y3)` are hardcoded to `(0, 0)` and `(1,1)` respectively.

-}
bezier : { x : Float, y : Float } -> { x : Float, y : Float } -> Float -> Float
bezier p1 p2 t =
    let
        lerp from to v =
            from + (to - from) * v

        pair interpolate ( a0, b0 ) ( a1, b1 ) v =
            ( interpolate a0 a1 v, interpolate b0 b1 v )

        casteljau ps =
            case ps of
                [ ( x, y ) ] ->
                    y

                xs ->
                    List.map2 (\x y -> pair lerp x y t) xs (Maybe.withDefault [] (List.tail xs))
                        |> casteljau
    in
    casteljau [ ( 0, 0 ), ( p1.x, p1.y ), ( p2.x, p2.y ), ( 1, 1 ) ]
