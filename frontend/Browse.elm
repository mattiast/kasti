port module Browse exposing (..)

import Html exposing (program)
import Json.Decode as D
import Json.Encode as JE
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD
import Helpers as H
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
    , Http.get "/feeds" (D.list H.decodeFeed)
        |> RD.sendRequest
        |> Cmd.map FeedsReceive
    )


getProgress : Episode -> Cmd (RD.WebData State)
getProgress ep =
    Http.get ("/progress/" ++ toString ep.id) (D.map (State ep) D.float)
        |> RD.sendRequest


getEpisodes : FeedId -> Cmd (RD.WebData (List Episode))
getEpisodes feed_id =
    Http.get ("/episodes/" ++ toString feed_id) (D.list H.decodeEpisode)
        |> RD.sendRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FeedsReceive stuff ->
            ( { model | feeds = stuff }, Cmd.none )

        EpisodePick ep ->
            ( model
            , getProgress ep
                |> Cmd.map ReceiveProgress
            )

        AskEpList feed_id ->
            ( { model | episodes = RD.Loading }
            , getEpisodes feed_id
                |> Cmd.map ReceiveEpList
            )

        ReceiveEpList stuff ->
            ( { model | episodes = stuff }, Cmd.none )

        ReceiveProgress stuff ->
            ( { model | progress = stuff |> Debug.log "rec prog" }
            , stuff
                |> RD.map (\s -> setCurrentTime s.time)
                |> RD.withDefault Cmd.none
            )

        ProgMsg (AskTime ep) ->
            ( model
            , getProgress ep
                |> Cmd.map ReceiveProgress
            )

        ProgMsg (PostTime s) ->
            ( model, postProgress s )

        ProgMsg (TimeUpdate t) ->
            ( { model
                | progress =
                    RD.map (\state -> { state | time = t }) model.progress
              }
            , Cmd.none
            )

        Nop ->
            ( Debug.log "nop" model, Cmd.none )

        SyncFeedAsk fid ->
            let
                newModel =
                    modifyFeedAtId fid (\feed -> { feed | syncState = RD.Loading }) model
            in
                ( newModel, syncFeed fid )

        SyncFeedReceive fid state ->
            let
                newModel =
                    modifyFeedAtId fid (\feed -> { feed | syncState = state }) model
            in
                ( newModel, Cmd.none )

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


postNewFeed : NewFeed -> Cmd Msg
postNewFeed newFeed =
    Http.post "/feed"
        (Http.jsonBody <| Debug.log "posting" <| H.encodeNewFeed newFeed)
        (D.succeed "")
        |> RD.sendRequest
        |> Cmd.map (RD.map (\_ -> ()) >> NewFeedReceive)


port setCurrentTime : Float -> Cmd msg


syncFeed : FeedId -> Cmd Msg
syncFeed fid =
    Http.get ("/syncfeed/" ++ toString fid) (D.succeed ())
        |> RD.sendRequest
        |> Cmd.map (SyncFeedReceive fid)


postProgress : State -> Cmd Msg
postProgress state =
    Http.post "/progress"
        (Http.jsonBody <| Debug.log "posting" <| H.encodeProgress state)
        (D.succeed "")
        |> RD.sendRequest
        |> Cmd.map (\_ -> Nop)

