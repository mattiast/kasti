module Helpers exposing (..)

import Date exposing (Date)
import Types exposing (..)
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as J
import Json.Encode as JE
import RemoteData as RD
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


encodeNewFeed : NewFeed -> JE.Value
encodeNewFeed newFeed =
    JE.object
        [ ( "name", JE.string newFeed.name )
        , ( "url", JE.string newFeed.url )
        ]


decodeFeed : J.Decoder Feed
decodeFeed =
    J.map4 Feed
        (J.index 0 J.int)
        (J.index 1 <| J.field "name" (J.string))
        (J.index 1 <| J.field "url" (J.string))
        (J.succeed RD.NotAsked)

onTimeUpdate : Attribute MsgProg
onTimeUpdate =
    on "timeupdate" (J.map (TimeUpdate) targetCurrentTime)


targetCurrentTime : J.Decoder Float
targetCurrentTime =
    J.at [ "target", "currentTime" ] J.float
