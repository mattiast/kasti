module Episode exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, program, div, button, text, audio, br, ul, li, a, tr, td, th, table)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick, on)
import Json.Decode as D
import Date exposing (Date)
import Date.Extra as Date
import Types exposing (..)


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
