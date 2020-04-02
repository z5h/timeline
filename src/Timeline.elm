module Timeline exposing
    ( Timeline, Msg, Status(..)
    , init, update, subscriptions, view, viewDocument, msg
    , value, transition, map, withDefault, sequence
    , currentTime
    , push
    , ease
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


# Integration helpers

@docs init, update, subscriptions, view, viewDocument, msg


# Rendering and animating during view

@docs value, transition, map, withDefault, sequence


# Utility

@docs currentTime


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
import Url



-- Stuff
--


type alias Event t =
    { time : Posix, value : t }


{-| Map the value part of an event.
-}
mapEvent : (t -> s) -> Event t -> Event s
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
    , key : Posix
    , flipFlop : Bit
    }


longAgo : Posix
longAgo =
    0 |> Time.millisToPosix


{-| Get the current time from a timeline.
Updated internally via `Browser.Events.onAnimationFrame`, thus inheriting its
resolution.

Having this prevents the need from tracking a high resolution time value in your model,
because doing such a thing would prohibit use of Timeline due to GC issues.

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
    let
        _ =
            Debug.log "Lazy" "View"
    in
    view_ (Timeline current history limit timestamp timestamp flipFlop)


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
map f { current, history, now, limit, flipFlop, key } =
    let
        ( first, rest ) =
            reverseNonEmpty ( current, history )

        mapf =
            mapEvent f

        initial =
            { current = mapf first, history = [], now = longAgo, key = longAgo, limit = limit, flipFlop = 0 }
    in
    List.foldl
        (\e t -> push (mapf e) t)
        initial
        rest
        |> (\timeline -> { timeline | now = now, key = key, flipFlop = flipFlop })


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
    , key = timeline.key
    , limit = timeline.limit
    , flipFlop = timeline.flipFlop
    }


{-| A transition status. Either `At` a value for a specific number of milliseconds,
or `Transitioning` from one value to another, with a "remaining" float value.

Note that during a transition, the float value goes from `1.0 -> 0.0`.
The "remaining" float value returned is not linear, but a dampened-spring
eased value. Use it directly (with a multiplier) to fade, scale, translate
with a natural and pleasant effect.

-}
type Status t
    = At t
    | Transitioning t t Float


ease : Status t -> Status t
ease status =
    case status of
        At t ->
            At t

        Transitioning t0 t1 f ->
            Transitioning t0 t1 (niceBezier f)


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


transitionHelper : Posix -> Int -> Event t -> List (Event t) -> Status t
transitionHelper now duration e events =
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
                    transitionHelper e.time duration prev rest
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
                    remaining (timeDiff now e.time) duration
                        - remaining_
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


niceBezier : Float -> Float
niceBezier f =
    bezier 0.78 0 0.22 1 f


bezier : Float -> Float -> Float -> Float -> Float -> Float
bezier x1 y1 x2 y2 time =
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
                    List.map2 (\x y -> pair lerp x y time) xs (Maybe.withDefault [] (List.tail xs))
                        |> casteljau
    in
    casteljau [ ( 0, 0 ), ( x1, y1 ), ( x2, y2 ), ( 1, 1 ) ]
