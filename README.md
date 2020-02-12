# Timeline
Convert static model changes into smooth transitions to animate.

## Example

See [this Ellie](https://ellie-app.com/82p3nvbZfMCa1) for a simple example. 

## Introduction

Timeline helps you animate the state changes of your `Model`.

It is designed to be very simple to integrate (a few minutes).

Once integrated, Timeline provides direct access to your model when needed.
It also provides access to how any part of your model has transitioned over time,
allowing you easily animate any property change.

**Important:** Timeline keeps a (garbage collected) history of your model states.
If your model updates at a modest pace then this API should be fine.

Specifically, when a pause between model changes exceeds the transition/animation duration,
then model history can be discarded.

If your model has very high churn (e.g. You're doing `Time.every 5`) then this is
not the API for you.


## Basic Setup

Integrating is simple. Once you understand the principles,
it should take only a few minutes of work.

Assuming your app's `main` looks something like:

```elm
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
```

it needs to be changed to:

```elm
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
```

In short, we are using functions in timeline to turn our program into a

```elm
Program () (Timeline Model) (Timeline.Msg Msg)
```

Note: The first two parameters to `Timeline.init` are the max duration of an animation (this is to aid garbage collection),
and the current time (safe to set to `0` for now, and discuss later).

Your `init`, `update` and `subscriptions` remain unchanged. 🎉

Your `view`, instead of having type

```elm
view : Model -> Html Msg
```

will now have type

```elm
view : Timeline Model -> Html Msg
```

A simple change to your view function should allow your program to run as it did:

```elm
view : Timeline Model -> Html Msg
view timeline =
    let
        model =
            Timeline.value timeline
    in
    ... -- your code as it used to work using model
```

Nothing exciting will happen yet, but now you have a `Timeline` at your disposal,
and this can be used to generate animations.