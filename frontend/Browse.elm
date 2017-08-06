port module Browse exposing (..)

import Html exposing (program)
import Json.Decode as D
import Json.Encode as JE
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD
import Episode as E
import Play as P
import Types exposing (..)
import Views


main : Program Never Model Msg
main =
    program
        { init = init
        , view = Views.view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Model RD.NotAsked emptyNewFeed RD.NotAsked RD.NotAsked
    , Http.get "/feeds" (D.list decodeFeed)
        |> RD.sendRequest
        |> Cmd.map FeedsReceive
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FeedsReceive stuff ->
            ( { model | feeds = stuff }
            , Cmd.none
            )

        EpMsg (E.Pick ep) ->
            ( model
            , Http.get ("/progress/" ++ toString ep.id) (D.map (P.State ep) D.float)
                |> RD.sendRequest
                |> Cmd.map ReceiveProgress
            )

        AskEpList feed_id ->
            ( { model | episodes = RD.Loading }
            , Http.get ("/episodes/" ++ toString feed_id) (D.list E.decodeEpisode)
                |> RD.sendRequest
                |> Cmd.map ReceiveEpList
            )

        ReceiveEpList stuff ->
            ( { model | episodes = stuff }, Cmd.none )

        ReceiveProgress stuff ->
            ( { model | progress = stuff |> Debug.log "rec prog" }
            , RD.map (\s -> setCurrentTime s.time) stuff
                |> RD.withDefault Cmd.none
            )

        ProgMsg (P.AskTime ep) ->
            ( model
            , Http.get ("/progress/" ++ toString ep.id) (D.map (P.State ep) D.float)
                |> RD.sendRequest
                |> Cmd.map ReceiveProgress
            )

        ProgMsg (P.PostTime s) ->
            ( model, postProgress s )

        ProgMsg m ->
            ( { model
                | progress =
                    RD.map (P.update m) model.progress
              }
            , Cmd.none
            )

        Nop ->
            ( Debug.log "nop" model, Cmd.none )

        SyncFeedAsk fid ->
            let
                upd : Feed -> Feed
                upd feed =
                    if feed.id == fid then
                        { feed | syncState = RD.Loading }
                    else
                        feed
            in
                ( { model | feeds = RD.map (List.map upd) model.feeds }
                , syncFeed fid
                )

        SyncFeedReceive fid state ->
            let
                upd : Feed -> Feed
                upd feed =
                    if feed.id == fid then
                        { feed | syncState = state }
                    else
                        feed
            in
                ( { model | feeds = RD.map (List.map upd) model.feeds }
                , Cmd.none
                )

        UpdateNewFeed newFeed ->
            ( { model | newFeed = newFeed }, Cmd.none )

        NewFeedPost ->
            ( model, postNewFeed model.newFeed )

        NewFeedReceive postStatus ->
            let
                oldNewFeed =
                    model.newFeed
            in
                ( { model | newFeed = { oldNewFeed | postStatus = postStatus } }, Cmd.none )


postNewFeed : NewFeed -> Cmd Msg
postNewFeed newFeed =
    Http.post "/feed"
        (Http.jsonBody <| Debug.log "posting" <| encodeNewFeed newFeed)
        (D.succeed "")
        |> RD.sendRequest
        |> Cmd.map (RD.map (\_ -> ()) >> NewFeedReceive)


port setCurrentTime : Float -> Cmd msg


encodeNewFeed : NewFeed -> JE.Value
encodeNewFeed newFeed =
    JE.object
        [ ( "name", JE.string newFeed.name )
        , ( "url", JE.string newFeed.url )
        ]


syncFeed : FeedId -> Cmd Msg
syncFeed fid =
    Http.get ("/syncfeed/" ++ toString fid) (D.succeed ())
        |> RD.sendRequest
        |> Cmd.map (SyncFeedReceive fid)


postProgress : P.State -> Cmd Msg
postProgress state =
    Http.post "/progress"
        (Http.jsonBody <| Debug.log "posting" <| P.encodeProgress state)
        (D.succeed "")
        |> RD.sendRequest
        |> Cmd.map (\_ -> Nop)


decodeFeed : D.Decoder Feed
decodeFeed =
    D.map4 Feed
        (D.index 0 D.int)
        (D.index 1 <| D.field "name" (D.string))
        (D.index 1 <| D.field "url" (D.string))
        (D.succeed RD.NotAsked)
