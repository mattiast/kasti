module Helpers exposing (..)

import Date exposing (Date)
import Types exposing (..)
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as J
import Json.Encode as JE
import Date.Extra as Date
import Types exposing (..)


decodeEpisode : J.Decoder Episode
decodeEpisode =
    J.map4 Episode
        (J.index 0 J.int)
        (J.index 1 <| J.field "title" J.string)
        (J.index 1 <| J.field "url" J.string)
        (J.index 1 <| J.field "date" decodeDate)


decodeDate : J.Decoder Date
decodeDate =
    let
        helper : String -> J.Decoder Date
        helper str =
            case Date.fromString str of
                Ok date ->
                    J.succeed date

                Err error ->
                    J.fail (toString error)
    in
        J.andThen helper J.string


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
