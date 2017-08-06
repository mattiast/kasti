module Play exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, div, button, text, audio, br, a, h3, h4, p, progress)
import Html.Attributes exposing (src, controls, style, id, href, class)
import Html.Events exposing (onClick, on)
import Json.Decode as J
import Json.Encode as JE
import Date.Extra as Date
import Episode as E
import Types exposing (..)


encodeProgress : State -> JE.Value
encodeProgress state =
    JE.object
        [ ( "episode_id", JE.int state.episode.id )
        , ( "position", JE.float state.time )
        ]


view : State -> Html MsgProg
view state =
    div []
        [ showState state
        , br [] []
        , audio
            [ src state.episode.url
            , controls True
            , style [ ( "width", "1000px" ) ]
            , id "audio-player"
            , onTimeUpdate
            ]
            []
        , br [] []
        , div [ class "field is-grouped" ]
            [ p [ class "control" ]
                [ a [ class "button is-primary", onClick (PostTime state) ]
                    [ text "Save position"
                    ]
                ]
            , p [ class "control" ]
                [ a [ class "button is-primary", onClick (AskTime state.episode) ]
                    [ text "Get position"
                    ]
                ]
            ]
        ]


showState : State -> Html MsgProg
showState state =
    div []
        [ h4 [ class "title is-4" ] [ text state.episode.title ]
        , p []
            [ text (Date.toFormattedString "y/M/d" state.episode.date)
            ]
            , a [ href state.episode.url ] [ text "[mp3 link]" ]
        ]


update : MsgProg -> State -> State
update msg state =
    case msg of
        TimeUpdate t ->
            { state | time = t }

        AskTime e ->
            state

        PostTime s ->
            state


onTimeUpdate : Attribute MsgProg
onTimeUpdate =
    on "timeupdate" (J.map (TimeUpdate) targetCurrentTime)


targetCurrentTime : J.Decoder Float
targetCurrentTime =
    J.at [ "target", "currentTime" ] J.float
