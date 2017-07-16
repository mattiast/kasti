module Browse exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, program, div, button, text, audio, br, ul, li, a)
import Html.Events exposing (onClick, on)
import Json.Decode as D
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD
import Episode as E
import Play as P
import Time


tick : Sub.Sub Msg
tick =
    Time.every (Time.second * 2) (\t -> Tick)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> tick
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
    | Tick
    | Nop


type alias Feed =
    { id : Int
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

        ProgMsg m ->
            ( { model
                | progress =
                    RD.map (P.update m) model.progress
                        |> Debug.log "msg prog"
              }
            , Cmd.none
            )

        Tick ->
            ( model
            , RD.map postProgress model.progress
                |> RD.withDefault Cmd.none
            )

        Nop ->
            ( model, Cmd.none |> Debug.log "nop" )


setCurrentTime : Float -> Cmd msg
setCurrentTime t =
    Cmd.none


postProgress : P.State -> Cmd Msg
postProgress state =
    Http.post "/progress"
        (Http.jsonBody <| Debug.log "tick" <| P.encodeProgress state)
        (D.list D.string)
        |> RD.sendRequest
        |> Cmd.map (\_ -> Nop)


view : Model -> Html Msg
view model =
    div []
        [ RD.map P.view model.progress
            |> RD.withDefault (div [] [])
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
