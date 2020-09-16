module Discrete.Status exposing (Status, fromTimelineStatus, toTimelineStatus)

import Timeline


type Status t
    = At t
    | Transitioning t t Float


toTimelineStatus : Status t -> Timeline.Status t
toTimelineStatus status =
    case status of
        At t ->
            Timeline.At t

        Transitioning from to f ->
            Timeline.Transitioning from to f


fromTimelineStatus : Timeline.Status t -> Status t
fromTimelineStatus timelineStatus =
    case timelineStatus of
        Timeline.At t ->
            At t

        Timeline.Transitioning from to f ->
            Transitioning from to f
