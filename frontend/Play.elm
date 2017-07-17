module Play exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, div, button, text, audio, br, a)
import Html.Attributes exposing (src, controls, style, id)
import Html.Events exposing (onClick, on)
import Json.Decode as J
import Json.Encode as JE
import Episode as E


type Msg
    = TimeUpdate Float
    | AskTime E.Episode
    | Complete


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
            , id "audio-player"
            ]
            []
        , br [] []
        , a [ onClick (TimeUpdate 120) ] [ text "Save position" ]
        , a [ onClick (AskTime state.episode) ] [ text "Get position" ]
        ]


update : Msg -> State -> State
update msg state =
    case msg of
        TimeUpdate t ->
            { state | time = t }

        Complete ->
            state
                |> Debug.log "complete tota"
        AskTime e -> state


onTimeUpdate : Attribute Msg
onTimeUpdate =
    on "timeupdate" (J.map TimeUpdate targetCurrentTime)


onAudioComplete : Attribute Msg
onAudioComplete =
    on "complete" (J.succeed Complete)


targetCurrentTime : J.Decoder Float
targetCurrentTime =
    J.at [ "target", "currentTime" ] J.float
