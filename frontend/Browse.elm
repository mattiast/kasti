module Browse exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, program, div, button, text, audio, br, ul, li, a)
import Html.Attributes exposing (src, controls, style, href)
import Html.Events exposing (onClick, on)
import Json.Decode as D
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD
import Episode as E


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
    }


type Msg
    = FeedsReceive (RD.WebData (List Feed))
    | EpMsg E.Msg
    | AskEpList Int
    | ReceiveEpList (RD.WebData (List E.Episode))


type alias Feed =
    { id : Int
    , name : String
    , url : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model RD.NotAsked RD.NotAsked
    , RD.sendRequest getFeeds
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
            ( model, Cmd.none )

        AskEpList feed_id ->
            ( { model | episodes = RD.Loading }
            , Http.get ("/episodes/" ++ toString feed_id) (D.list E.decodeEpisode)
                |> RD.sendRequest
                |> Cmd.map ReceiveEpList
            )

        ReceiveEpList stuff ->
            ( { model | episodes = stuff }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ feedList model.feeds
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


getFeeds : Http.Request (List Feed)
getFeeds =
    Http.get "/feeds" (D.list decodeFeed)


decodeFeed : D.Decoder Feed
decodeFeed =
    D.map3 Feed
        (D.index 0 D.int)
        (D.index 1 <| D.field "name" (D.string))
        (D.index 1 <| D.field "url" (D.string))
