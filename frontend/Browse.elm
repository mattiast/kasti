port module Browse exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, program, div, button, text, audio, br, ul, li, a)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (id, style)
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

type alias FeedId = Int

type alias Feed =
    { id : FeedId
    , name : String
    , url : String
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
            ( model , postProgress s )

        ProgMsg m ->
            ( { model
                | progress =
                    RD.map (P.update m) model.progress
              }
            , Cmd.none
            )

        Nop ->
            ( model, Cmd.none )


port setCurrentTime : Float -> Cmd msg


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
        [ RD.map P.view model.progress
            |> RD.withDefault (audio [ id "audio-player", style [ ( "display", "none" ) ] ] [])
            |> Html.map ProgMsg
        , feedList model.feeds
        , model.episodes
            |> RD.map E.episodeList
            |> RD.withDefault (text (toString model.episodes))
            |> Html.map EpMsg
        ]


feedList : RD.WebData (List Feed) -> Html Msg
feedList feeds =
    case feeds of
        RD.Success fs ->
            let
                feedItem f =
                    li [] [ a [ onClick (AskEpList f.id) ] [ text f.name ] ]
            in
                fs
                    |> List.map feedItem
                    |> ul []

        _ ->
            text (toString feeds)


decodeFeed : D.Decoder Feed
decodeFeed =
    D.map3 Feed
        (D.index 0 D.int)
        (D.index 1 <| D.field "name" (D.string))
        (D.index 1 <| D.field "url" (D.string))
