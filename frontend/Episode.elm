module Episode exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, program, div, button, text, audio, br, ul, li, a)
import Html.Events exposing (onClick, on)
import Json.Decode as D
import Date exposing (Date)
import Date.Extra as Date


type alias Episode =
    { id : Int
    , title : String
    , url : String
    , date : Date
    }


decodeEpisode : D.Decoder Episode
decodeEpisode =
    D.map4 Episode
        (D.index 0 D.int)
        (D.index 1 <| D.field "title" D.string)
        (D.index 1 <| D.field "url" D.string)
        (D.index 1 <| D.field "date" decodeDate)


decodeDate : D.Decoder Date
decodeDate =
    let
        helper : String -> D.Decoder Date
        helper str =
            case Date.fromString str of
                Ok date ->
                    D.succeed date

                Err error ->
                    D.fail (toString error)
    in
        D.andThen helper D.string


type Msg
    = Pick Episode


episodeList : List Episode -> Html Msg
episodeList eps =
    eps
        |> List.sortWith (\x y -> Date.compare y.date x.date)
        |> List.map (\e -> li [] [ a [ onClick (Pick e) ] [ text e.title ] ])
        |> ul []
