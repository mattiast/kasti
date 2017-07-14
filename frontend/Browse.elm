module Browse exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, program, div, button, text, audio, br)
import Html.Attributes exposing (src, controls, style)
import Html.Events exposing (onClick, on)
import Json.Decode as D
import Http
import Platform.Cmd as Cmd
import Platform.Sub as Sub


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model
    = Model Int


type Msg
    = SetValue Int
    | FeedsReceive (Result Http.Error (List Feed))

init : (Model, Cmd Msg)
init = ( Model 0, Http.send FeedsReceive getFeeds )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetValue x ->
            (Model x, Cmd.none)

        FeedsReceive _ ->
            (model, Cmd.none)


view : Model -> Html Msg
view (Model x) =
    text (toString x)


getFeeds : Http.Request (List Feed)
getFeeds =
    Http.get "/feeds" (D.list decodeFeed)


type Feed
    = Feed String String


feedName : Feed -> String
feedName (Feed n _) =
    n


feedUrl : Feed -> String
feedUrl (Feed _ u) =
    u


decodeFeed : D.Decoder Feed
decodeFeed =
    D.map2 Feed
        (D.field "name" (D.string))
        (D.field "url" (D.string))
