module Play exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, div, button, text, audio, br, a, h3, h4, p, progress)
import Html.Attributes exposing (src, controls, style, id, href, class)
import Html.Events exposing (onClick, on)
import Json.Decode as J
import Json.Encode as JE
import Date.Extra as Date
import Episode as E
import Types exposing (..)


encodeProgress : State -> JE.Value
encodeProgress state =
    JE.object
        [ ( "episode_id", JE.int state.episode.id )
        , ( "position", JE.float state.time )
        ]


update : MsgProg -> State -> State
update msg state =
    case msg of
        TimeUpdate t ->
            { state | time = t }

        AskTime e ->
            state

        PostTime s ->
            state


onTimeUpdate : Attribute MsgProg
onTimeUpdate =
    on "timeupdate" (J.map (TimeUpdate) targetCurrentTime)


targetCurrentTime : J.Decoder Float
targetCurrentTime =
    J.at [ "target", "currentTime" ] J.float
