module Play exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, div, button, text, audio, br)
import Html.Attributes exposing (src, controls, style)
import Html.Events exposing (onClick, on)
import Json.Decode as Json


audioUrl : String
audioUrl =
    "http://feeds.soundcloud.com/stream/322754602-stack-exchange-stack-overflow-podcast-109.mp3"


type alias Msg =
    Float


type alias Progress =
    Float


view : Progress -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ]
        , br [] []
        , audio
            [ src audioUrl
            , controls True
            , style [ ( "width", "1000px" ) ]
            , onTimeUpdate
            ]
            []
        ]


onTimeUpdate : Attribute Msg
onTimeUpdate =
    on "timeupdate" targetCurrentTime


targetCurrentTime : Json.Decoder Float
targetCurrentTime =
    Json.at [ "target", "currentTime" ] Json.float
