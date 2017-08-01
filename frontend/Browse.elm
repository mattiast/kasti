port module Browse exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (id, style, href, class)
import Json.Decode as D
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD
import Episode as E
import Play as P


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { feeds : RD.WebData (List Feed)
    , episodes : RD.WebData (List E.Episode)
    , progress : RD.WebData P.State
    }


type Msg
    = FeedsReceive (RD.WebData (List Feed))
    | EpMsg E.Msg
    | AskEpList Int
    | ReceiveEpList (RD.WebData (List E.Episode))
    | ReceiveProgress (RD.WebData P.State)
    | ProgMsg P.Msg
    | Nop
    | SyncFeedAsk FeedId
    | SyncFeedReceive FeedId (RD.WebData ())


type alias FeedId =
    Int


type alias Feed =
    { id : FeedId
    , name : String
    , url : String
    , syncState : RD.WebData ()
    }


init : ( Model, Cmd Msg )
init =
    ( Model RD.NotAsked RD.NotAsked RD.NotAsked
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
            ( model, Cmd.none )

        SyncFeedAsk fid ->
            let
                upd : Feed -> Feed
                upd feed =
                    if feed.id == fid then
                        Debug.log "asked" { feed | syncState = RD.Loading }
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
                        Debug.log "received" { feed | syncState = state }
                    else
                        feed
            in
                ( { model | feeds = RD.map (List.map upd) model.feeds }
                , Cmd.none
                )


port setCurrentTime : Float -> Cmd msg


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


view : Model -> Html Msg
view model =
    div []
        [ navbar
        , RD.map P.view model.progress
            |> RD.withDefault (audio [ id "audio-player", style [ ( "display", "none" ) ] ] [])
            |> Html.map ProgMsg
        , feedList model.feeds
        , model.episodes
            |> RD.map E.episodeList
            |> RD.withDefault (text (toString model.episodes))
            |> Html.map EpMsg
        ]


navbar : Html Msg
navbar =
    nav [ class "navbar" ]
        [ div [ class "navbar-brand" ]
            [ span [ class "icon is-large" ] [ i [ class "fa fa-google-wallet" ] [] ]
            , span [ class "tag is-primary is-large" ] [ text "KASTI" ]
            ]
        ]


feedList : RD.WebData (List Feed) -> Html Msg
feedList feeds =
    case feeds of
        RD.Success fs ->
            let
                feedItem f =
                    tr []
                        [ td [ onClick (AskEpList f.id) ] [ a [] [ text f.name ] ]
                        , td [] [ syncButton f ]
                        ]
            in
                fs
                    |> List.map feedItem
                    |> table [ class "table is-narrow" ]

        _ ->
            text (toString feeds)


syncButton : Feed -> Html Msg
syncButton feed =
    let
        aClass =
            case feed.syncState of
                RD.Loading ->
                    "button is-loading"

                RD.Success () ->
                    "button is-success"

                RD.NotAsked ->
                    "button is-small"

                RD.Failure e ->
                    "button is-danger"
    in
        a [ class aClass, onClick (SyncFeedAsk feed.id) ]
            [ span [ class "icon is-small" ]
                [ i [ class "fa fa-refresh" ] []
                ]
            ]


decodeFeed : D.Decoder Feed
decodeFeed =
    D.map4 Feed
        (D.index 0 D.int)
        (D.index 1 <| D.field "name" (D.string))
        (D.index 1 <| D.field "url" (D.string))
        (D.succeed RD.NotAsked)
