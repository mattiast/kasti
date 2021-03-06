port module Main exposing (main)

import Browser
import Browser.Navigation as N
import Client
import Helpers as H
import Json.Decode as D
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD
import Types exposing (..)
import Url as U
import Views


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = Views.view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChange
        , onUrlRequest = ClickUrl
        }


init : D.Value -> U.Url -> N.Key -> ( Model, Cmd Msg )
init _ loc key =
    let
        firstView =
            parseView loc.path
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
      , navKey = key
      , positions = RD.NotAsked
      , posSortBy = ByDate
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
            ( { model | playerState = stuff }
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

        ProgMsg (SetDoubleSpeed isDouble) ->
            ( { model
                | playerState =
                    RD.map (\state -> { state | doubleSpeed = isDouble }) model.playerState
              }
            , setPlaybackRate
                (if isDouble then
                    2.0

                 else
                    1.0
                )
            )

        Nop ->
            ( model, Cmd.none )

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
                    parseView loc.path
                        |> Maybe.withDefault model.view
              }
            , Cmd.none
            )

        ClickUrl urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , N.pushUrl model.navKey (U.toString url)
                    )

                Browser.External url ->
                    ( model
                    , N.load url
                    )

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
            ( { model | posSortBy = by }, Cmd.none )


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


port setPlaybackRate : Float -> Cmd msg
