module Play exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, div, button, text, audio, br)
import Html.Attributes exposing (src, controls, style)
import Html.Events exposing (onClick, on)
import Json.Decode as J
import Json.Encode as JE
import Episode as E


type Msg
    = TimeUpdate Float


type alias State =
    { episode : E.Episode
    , time : Float
    }


encodeProgress : State -> JE.Value
encodeProgress state =
    JE.object
        [ ( "episode_id", JE.int state.episode.id )
        , ( "position", JE.float state.time )
        ]


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
update msg state =
    case msg of
        TimeUpdate t ->
            { state | time = t }


onTimeUpdate : Attribute Msg
onTimeUpdate =
    on "timeupdate" (J.map TimeUpdate targetCurrentTime)


targetCurrentTime : J.Decoder Float
targetCurrentTime =
    J.at [ "target", "currentTime" ] J.float
