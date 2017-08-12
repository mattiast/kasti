module Helpers exposing (..)

import Date exposing (Date)
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as J
import Json.Encode as JE
import RemoteData as RD
import Types exposing (..)
import Time.DateTime as T


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


encodeProgress : PlayerState -> JE.Value
encodeProgress state =
    Debug.log "progress encode" <|
        JE.object
            [ ( "episode_id", JE.int state.episode.id )
            , ( "position", JE.float state.time )
            , ( "duration", JE.float state.duration )
            ]


decodePosDur : J.Decoder ( Float, Float )
decodePosDur =
    J.map2 (\x y -> ( x, y ))
        (J.field "position" J.float)
        (J.field "duration" J.float)


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
    on "timeupdate" (J.map2 TimeUpdate targetCurrentTime targetDuration)


targetCurrentTime : J.Decoder Float
targetCurrentTime =
    J.at [ "target", "currentTime" ] J.float


targetDuration : J.Decoder Float
targetDuration =
    J.at [ "target", "duration" ] J.float


modifyFeedAtId : FeedId -> (Feed -> Feed) -> Model -> Model
modifyFeedAtId fid upd model =
    let
        updAt : Feed -> Feed
        updAt feed =
            if feed.id == fid then
                upd feed
            else
                feed
    in
        { model | feeds = RD.map (List.map updAt) model.feeds }


renderSeconds : Float -> String
renderSeconds sec =
    let
        z =
            T.dateTime T.zero

        x =
            T.addSeconds 6832 z
    in
        toString (T.hour x) ++ ":" ++ toString (T.minute x) ++ ":" ++ toString (T.second x)
