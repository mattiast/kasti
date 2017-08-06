port module Browse exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on, onInput)
import Html.Attributes exposing (id, style, href, class, type_, name, placeholder)
import Json.Decode as D
import Json.Encode as JE
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
    , newFeed : NewFeed
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
    | UpdateNewFeed NewFeed
    | NewFeedPost
    | NewFeedReceive (RD.WebData ())


type alias FeedId =
    Int


type alias Feed =
    { id : FeedId
    , name : String
    , url : String
    , syncState : RD.WebData ()
    }


type alias NewFeed =
    { name : String
    , url : String
    , postStatus : RD.WebData ()
    }


emptyNewFeed : NewFeed
emptyNewFeed =
    NewFeed "" "" RD.NotAsked


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


view : Model -> Html Msg
view model =
    div []
        [ navbar model.newFeed
        , RD.map P.view model.progress
            |> RD.withDefault (audio [ id "audio-player", style [ ( "display", "none" ) ] ] [])
            |> Html.map ProgMsg
        , div [ class "columns" ]
            [ div [ class "column is-one-quarter" ]
                [ feedList model.feeds
                ]
            , div [ class "column" ]
                [ model.episodes
                    |> RD.map E.episodeList
                    |> RD.withDefault (text (toString model.episodes))
                    |> Html.map EpMsg
                ]
            ]
        ]


navbar : NewFeed -> Html Msg
navbar newFeed =
    nav [ class "navbar" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ]
                [ span [ class "icon is-large" ] [ i [ class "fa fa-google-wallet" ] [] ]
                , span [ class "tag is-primary is-large" ] [ text "KASTI" ]
                ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ div [ class "navbar-item has-dropdown is-hoverable" ]
                    [ a [ class "navbar-link is-active" ] [ text "Add feed" ]
                    , div [ class "navbar-dropdown" ]
                        [ div [ class "navbar-item" ]
                            [ div [ class "field" ]
                                [ label [ class "label" ] [ text "Name" ]
                                , div [ class "control" ]
                                    [ input
                                        [ type_ "text"
                                        , name "name"
                                        , placeholder "Feed title"
                                        , onInput (\v -> UpdateNewFeed { newFeed | name = v })
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , div [ class "navbar-item" ]
                            [ div [ class "field" ]
                                [ label [ class "label" ] [ text "Feed URL" ]
                                , div [ class "control" ]
                                    [ input
                                        [ type_ "url"
                                        , name "url"
                                        , placeholder "http://"
                                        , onInput (\v -> UpdateNewFeed { newFeed | url = v })
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , div [ class "navbar-item" ]
                            [ div [ class "field" ]
                                [ div [ class "control" ]
                                    [ button [ class (addFeedButtonClass newFeed), onClick NewFeedPost ]
                                        [ text "Add" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


addFeedButtonClass : NewFeed -> String
addFeedButtonClass newFeed =
    case newFeed.postStatus of
        RD.NotAsked ->
            "button is-primary"

        RD.Loading ->
            "button is-primary is-loading"

        RD.Success _ ->
            "button is-success"

        RD.Failure _ ->
            "button is-danger"


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
            "button is-small"
                ++ case feed.syncState of
                    RD.NotAsked ->
                        ""

                    RD.Loading ->
                        " is-loading"

                    RD.Success () ->
                        " is-success"

                    RD.Failure e ->
                        " is-danger"
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
