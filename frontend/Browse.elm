module Browse exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, program, div, button, text, audio, br)
import Html.Attributes exposing (src, controls, style)
import Html.Events exposing (onClick, on)
import Json.Decode as D
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData as RD


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { feeds : RD.WebData (List Feed)
    }


type Msg
    = FeedsReceive (RD.WebData (List Feed))


init : ( Model, Cmd Msg )
init =
    ( Model RD.NotAsked
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


view : Model -> Html Msg
view model =
    text (toString model)


getFeeds : Http.Request (List Feed)
getFeeds =
    Http.get "/feeds" (D.list decodeFeed)


type alias Feed =
    { id : Int
    , name : String
    , url : String
    }




decodeFeed : D.Decoder Feed
decodeFeed =
    D.map3 Feed
        (D.index 0 D.int)
        (D.index 1 <| D.field "name" (D.string))
        (D.index 1 <| D.field "url" (D.string))
