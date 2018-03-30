port module Browse exposing (..)

import Json.Decode as D
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD
import Helpers as H
import Types exposing (..)
import Client
import Client.Types as C
import Views
import Navigation as N
import Date


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
        ( { feeds = RD.NotAsked
          , episodes = RD.NotAsked
          , playerState = RD.NotAsked
          , menuState =
                { newFeed = emptyNewFeed
                , syncAllState = RD.NotAsked
                , navbarActive = False
                }
          , view = firstView
          , positions = RD.NotAsked
          , newEpisodes = RD.NotAsked
          }
        , Cmd.batch
            [ Client.getFeeds
                |> Cmd.map FeedsReceive
            , Client.getPositions
                |> Cmd.map PositionsReceive
            , Client.getNewEpisodes
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
            , Client.getProgress ep
                |> Cmd.map ReceiveProgress
            )

        AskEpList feed_id ->
            ( { model | episodes = RD.Loading }
            , Client.getEpisodes feed_id
                |> Cmd.map ReceiveEpList
            )

        ReceiveEpList stuff ->
            ( { model | episodes = stuff }, Cmd.none )

        ReceiveProgress stuff ->
            ( { model | playerState = stuff |> Debug.log "rec prog" }
            , stuff
                |> RD.map (\s -> setCurrentTime s.time)
                |> RD.withDefault Cmd.none
            )

        ProgMsg (AskTime ep) ->
            ( model
            , Client.getProgress ep
                |> Cmd.map ReceiveProgress
            )

        ProgMsg (PostTime s) ->
            ( model, Client.postProgress s )

        ProgMsg (TimeUpdate pos dur) ->
            ( { model
                | playerState =
                    RD.map (\state -> { state | time = pos, duration = dur }) model.playerState
              }
            , Cmd.none
            )

        Nop ->
            ( Debug.log "nop" model, Cmd.none )

        SyncFeedAsk sfid ->
            let
                newModel =
                    case sfid of
                        SyncSingle fid ->
                            H.modifyFeedAtId fid (\feed -> { feed | syncState = RD.Loading }) model

                        SyncAll ->
                            H.modifyMenuState (\state -> { state | syncAllState = RD.Loading }) model
            in
                ( newModel, Client.syncFeed sfid )

        SyncFeedReceive (SyncSingle fid) state ->
            let
                newModel =
                    H.modifyFeedAtId fid (\feed -> { feed | syncState = state }) model

                nextCmd =
                    Client.getEpisodes fid
                        |> Cmd.map ReceiveEpList
            in
                ( newModel, nextCmd )

        SyncFeedReceive SyncAll state ->
            let
                newModel =
                    H.modifyMenuState (\ms -> { ms | syncAllState = state }) model
            in
                ( newModel, Cmd.none )

        UpdateNewFeed newFeed ->
            ( H.modifyMenuState (\state -> { state | newFeed = newFeed }) model
            , Cmd.none
            )

        NewFeedPost ->
            ( model, Client.postNewFeed model.menuState.newFeed )

        NewFeedReceive postStatus ->
            let
                oldNewFeed =
                    model.menuState.newFeed

                upd ms =
                    { ms | newFeed = { oldNewFeed | postStatus = postStatus } }
            in
                ( H.modifyMenuState upd model
                , Cmd.none
                )

        PositionsAsk ->
            ( model, Cmd.map PositionsReceive Client.getPositions )

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

        NavbarToggle ->
            let
                upd ms =
                    { ms | navbarActive = not ms.navbarActive }
            in
                ( H.modifyMenuState upd model
                , Cmd.none
                )

        SortBy by ->
            let
                f =
                    case by of
                        ByFeed ->
                            List.sortBy (\pi -> pi.ftitle)

                        ByDate ->
                            List.sortBy (\pi -> Date.toTime pi.episode.date)

                        ByTime ->
                            List.sortBy (\pi -> pi.duration - pi.position)
            in
                ( H.modifyPositions f model, Cmd.none )


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


port setCurrentTime : Float -> Cmd msg
