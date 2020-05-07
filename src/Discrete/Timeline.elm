module Discrete.Timeline exposing
    ( Timeline, Msg, Status
    , init, update, subscriptions, view, viewDocument, msg
    , value, transition, map, withDefault, sequence
    , currentTime
    , push
    , Playback(..), css, definitely, transitions, unwrap
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
import Discrete.Status
import Html exposing (Html)
import Html.Attributes
import Html.Lazy
import Internal.Util exposing (..)
import Time exposing (Posix)
import Timeline
import Url


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
type Timeline t
    = Timeline (Timeline.Timeline t)


unwrap : Timeline t -> Timeline.Timeline t
unwrap (Timeline timeline) =
    timeline


{-| Get the current time from a timeline.
Updated internally via `Browser.Events.onAnimationFrame`, thus inheriting its
resolution.

Having this prevents the need from tracking a high resolution time value in your model,
because doing such a thing would prohibit use of Timeline due to GC issues.

-}
currentTime : Timeline t -> Posix
currentTime =
    unwrap >> Timeline.currentTime


{-| A timeline Msg.
-}
type alias Msg m =
    Timeline.Msg m


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
init limit =
    Timeline.init limit >> Tuple.mapFirst Timeline


{-| Converts your existing `update` to a Timeline compatible `update`.

See Basic Setup.

-}
update : (msg -> model -> ( model, Cmd msg )) -> (Msg msg -> Timeline model -> ( Timeline model, Cmd (Msg msg) ))
update update_ msg_ (Timeline timeline) =
    Timeline.update update_ msg_ timeline |> Tuple.mapFirst Timeline


{-| Converts your existing `subscriptions` to a Timeline compatible `subscriptions`.

See Basic Setup.

-}
subscriptions : (model -> Sub msg) -> Timeline model -> Sub (Msg msg)
subscriptions subscriptions_ (Timeline timeline) =
    Timeline.subscriptions subscriptions_ timeline


{-| Convenience function.

Converts your `view : Timeline model -> Html msg` to
a `Timeline model -> Html (Timeline.Msg msg)` for use in your `main`.

-}
css : String -> Html msg
css string =
    Html.node "link"
        [ Html.Attributes.attribute "rel" "stylesheet"
        , Html.Attributes.attribute "href" ("data:text/css," ++ Url.percentEncode string)
        ]
        []


view : (Timeline model -> Html msg) -> Timeline model -> Html (Msg msg)
view view_ (Timeline timeline_) =
    Html.Lazy.lazy5 lazyRender
        view_
        timeline_.current
        timeline_.history
        timeline_.limit
        timeline_.flipFlop
        |> Html.map msg


lazyRender view_ current history limit flipFlop =
    let
        _ =
            Debug.log "Lazy" "Render"
    in
    Html.div []
        [ css keyframes
        , view_ (Timeline <| Timeline.Timeline current history limit current.time current.time flipFlop)
        ]


{-| Convenience function.

Converts your `view : Timeline model -> Browser.Document msg` to
a `Timeline model -> Browser.Document (Timeline.Msg msg)` for use in your
`main`.

-}
viewDocument : (Timeline model -> Browser.Document msg) -> Timeline model -> Browser.Document (Msg msg)
viewDocument view_ =
    view_
        >> (\document ->
                { title = document.title
                , body = (css keyframes :: document.body) |> List.map (Html.map msg)
                }
           )


{-| Maps a `msg` to a `Timeline.Msg msg`.

Useful in your `main` when defining things such as:

    onUrlChange =
        Timeline.msg << OnUrlChange

-}
msg : m -> Msg m
msg =
    Timeline.msg


{-| Exposed for testing. You don't need this.
-}
push : Event t -> Timeline t -> Timeline t
push e =
    unwrap >> Timeline.push e >> Timeline


{-| Extract the current (most recent) value from the timeline.
-}
value : Timeline t -> t
value =
    unwrap >> Timeline.value


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
map f =
    unwrap >> Timeline.map f >> Timeline


definitely : Timeline (Maybe t) -> Maybe (Timeline t)
definitely =
    unwrap >> Timeline.definitely >> Maybe.map Timeline


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
withDefault t =
    unwrap >> Timeline.withDefault t >> Timeline


{-| A transition status. Either `At` a value for a specific number of milliseconds,
or `Transitioning` from one value to another, with a "remaining" float value.

Note that during a transition, the float value goes from `1.0 -> 0.0`.
The "remaining" float value returned is not linear, but a dampened-spring
eased value. Use it directly (with a multiplier) to fade, scale, translate
with a natural and pleasant effect.

-}
type alias Status t =
    Discrete.Status.Status t


transition : Playback -> Int -> Timeline t -> Status t
transition playback duration timeline =
    timeline
        |> unwrap
        |> Timeline.transition (playbackForTimeline playback) duration
        |> Discrete.Status.fromTimelineStatus


transitions : Int -> Timeline t -> ( Status (Maybe t), List (Status (Maybe t)) )
transitions duration =
    unwrap
        >> Timeline.transitions duration
        >> (\( first, rest ) ->
                ( Discrete.Status.fromTimelineStatus first
                , List.map Discrete.Status.fromTimelineStatus rest
                )
           )


type Playback
    = Reversible
    | Interruptible
    | Restartable


playbackForTimeline : Playback -> Timeline.Playback
playbackForTimeline p =
    case p of
        Reversible ->
            Timeline.Reversible

        Interruptible ->
            Timeline.Interruptible

        Restartable ->
            Timeline.Restartable


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
sequence id =
    unwrap >> Timeline.sequence id >> List.map Timeline


niceBezierString =
    "cubic-bezier(0.78,0,0.22,1)"
