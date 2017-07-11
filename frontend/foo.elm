-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (beginnerProgram, div, button, text, audio, br)
import Html.Attributes exposing (src, controls, style)
import Html.Events exposing (onClick)

audioUrl : String
audioUrl = "http://feeds.soundcloud.com/stream/322754602-stack-exchange-stack-overflow-podcast-109.mp3"

main =
  beginnerProgram { model = 0, view = view, update = update }


view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    , br [] []
    , audio [ src audioUrl, controls True, style [("width", "1000px")] ] []
    ]


type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

