module Play exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, div, button, text, audio, br)
import Html.Attributes exposing (src, controls, style)
import Html.Events exposing (onClick, on)
import Json.Decode as Json
import Episode as E


type alias Msg =
    Float


type alias State =
    { episode : E.Episode
    , time : Float
    }


view : State -> Html Msg
view state =
    div []
        [ div [] [ text (toString state) ]
        , br [] []
        , audio
            [ src state.episode.url
            , controls True
            , style [ ( "width", "1000px" ) ]
            , onTimeUpdate
            ]
            []
        ]

update : Msg -> State -> State
update t state = { state | time = t }

onTimeUpdate : Attribute Msg
onTimeUpdate =
    on "timeupdate" targetCurrentTime


targetCurrentTime : Json.Decoder Float
targetCurrentTime =
    Json.at [ "target", "currentTime" ] Json.float
