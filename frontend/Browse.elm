port module Browse exposing (..)

import Json.Decode as D
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD
import Helpers as H
import Types exposing (..)
import Views
import Navigation as N


main : Program Never Model Msg
main =
    N.program UrlChange
        { init = init
        , view = Views.view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : N.Location -> ( Model, Cmd Msg )
init loc =
    let
        firstView =
            parseView loc.pathname
                |> Maybe.withDefault Browse
    in
        ( Model RD.NotAsked
            emptyNewFeed
            RD.NotAsked
            RD.NotAsked
            RD.NotAsked
            firstView
            RD.NotAsked
        , Cmd.batch
            [ getFeeds
                |> Cmd.map FeedsReceive
            , getPositions
                |> Cmd.map PositionsReceive
            , getNewEpisodes
                |> Cmd.map NewEpisodesReceive
            ]
        )


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

        ProgMsg (TimeUpdate pos dur) ->
            ( { model
                | progress =
                    RD.map (\state -> { state | time = pos, duration = dur }) model.progress
              }
            , Cmd.none
            )

        Nop ->
            ( Debug.log "nop" model, Cmd.none )

        SyncFeedAsk fid ->
            let
                newModel =
                    H.modifyFeedAtId fid (\feed -> { feed | syncState = RD.Loading }) model
            in
                ( newModel, syncFeed fid )

        SyncFeedReceive fid state ->
            let
                newModel =
                    H.modifyFeedAtId fid (\feed -> { feed | syncState = state }) model
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

        PositionsAsk ->
            ( model, Cmd.map PositionsReceive getPositions )

        PositionsReceive positions ->
            ( { model | positions = positions }, Cmd.none )

        UrlChange loc ->
            ( { model
                | view =
                    parseView (loc.pathname)
                        |> Maybe.withDefault model.view
              }
            , Cmd.none
            )

        ClickUrl url ->
            ( model, N.newUrl url )

        NewEpisodesReceive stuff ->
            ( { model | newEpisodes = stuff }, Cmd.none )


parseView : String -> Maybe WhichView
parseView url =
    case url of
        "/browse" ->
            Just Browse

        "/continue" ->
            Just Continue

        "/new" ->
            Just New

        _ ->
            Nothing


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


postProgress : PlayerState -> Cmd Msg
postProgress state =
    Http.post "/progress"
        (Http.jsonBody <| Debug.log "posting" <| H.encodeProgress state)
        (D.succeed "")
        |> RD.sendRequest
        |> Cmd.map (\_ -> PositionsAsk)


getFeeds : Cmd (RD.WebData (List Feed))
getFeeds =
    Http.get "/feeds" (D.list H.decodeFeed)
        |> RD.sendRequest


getProgress : Episode -> Cmd (RD.WebData PlayerState)
getProgress ep =
    Http.get ("/progress/" ++ toString ep.id) (D.map (\( pos, dur ) -> PlayerState ep pos dur) H.decodePosDur)
        |> RD.sendRequest


getPositions : Cmd (RD.WebData (List ProgressInfo))
getPositions =
    let
        decodeStuff =
            D.list <|
                D.map4 ProgressInfo
                    (D.index 0 D.string)
                    (D.index 1 H.decodeEpisode)
                    (D.index 2 <| D.field "position" D.float)
                    (D.index 2 <| D.field "duration" D.float)
    in
        Http.get "/progress/all"
            decodeStuff
            |> RD.sendRequest


getEpisodes : FeedId -> Cmd (RD.WebData (List Episode))
getEpisodes feed_id =
    Http.get ("/episodes/" ++ toString feed_id) (D.list H.decodeEpisode)
        |> RD.sendRequest


getNewEpisodes : Cmd (RD.WebData (List NewEpisode))
getNewEpisodes =
    let
        decodeStuff =
            D.list <|
                D.map2 NewEpisode
                    (D.index 0 D.string)
                    (D.index 1 H.decodeEpisode)
    in
        Http.get ("/episodes/new") decodeStuff
            |> RD.sendRequest
