module Views exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on, onInput)
import Html.Attributes exposing (id, style, href, class, type_, name, placeholder)
import Types exposing (..)
import RemoteData as RD
import Episode as E
import Play as P

view : Model -> Html Msg
view model =
    div []
        [ navbar model.newFeed
        , RD.map P.view model.progress
            |> RD.withDefault (audio [ id "audio-player", style [ ( "display", "none" ) ] ] [])
            |> Html.map ProgMsg
        , div [ class "columns" ]
            [ div [ class "column is-one-quarter" ]
                [ feedList model.feeds
                ]
            , div [ class "column" ]
                [ model.episodes
                    |> RD.map E.episodeList
                    |> RD.withDefault (text (toString model.episodes))
                    |> Html.map EpMsg
                ]
            ]
        ]


navbar : NewFeed -> Html Msg
navbar newFeed =
    nav [ class "navbar" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ]
                [ span [ class "icon is-large" ] [ i [ class "fa fa-google-wallet" ] [] ]
                , span [ class "tag is-primary is-large" ] [ text "KASTI" ]
                ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ div [ class "navbar-item has-dropdown is-hoverable" ]
                    [ a [ class "navbar-link is-active" ] [ text "Add feed" ]
                    , div [ class "navbar-dropdown" ]
                        [ div [ class "navbar-item" ]
                            [ div [ class "field" ]
                                [ label [ class "label" ] [ text "Name" ]
                                , div [ class "control" ]
                                    [ input
                                        [ type_ "text"
                                        , name "name"
                                        , placeholder "Feed title"
                                        , onInput (\v -> UpdateNewFeed { newFeed | name = v })
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , div [ class "navbar-item" ]
                            [ div [ class "field" ]
                                [ label [ class "label" ] [ text "Feed URL" ]
                                , div [ class "control" ]
                                    [ input
                                        [ type_ "url"
                                        , name "url"
                                        , placeholder "http://"
                                        , onInput (\v -> UpdateNewFeed { newFeed | url = v })
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , div [ class "navbar-item" ]
                            [ div [ class "field" ]
                                [ div [ class "control" ]
                                    [ button [ class (addFeedButtonClass newFeed), onClick NewFeedPost ]
                                        [ text "Add" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


addFeedButtonClass : NewFeed -> String
addFeedButtonClass newFeed =
    case newFeed.postStatus of
        RD.NotAsked ->
            "button is-primary"

        RD.Loading ->
            "button is-primary is-loading"

        RD.Success _ ->
            "button is-success"

        RD.Failure _ ->
            "button is-danger"


feedList : RD.WebData (List Feed) -> Html Msg
feedList feeds =
    case feeds of
        RD.Success fs ->
            let
                feedItem f =
                    tr []
                        [ td [ onClick (AskEpList f.id) ] [ a [] [ text f.name ] ]
                        , td [] [ syncButton f ]
                        ]
            in
                fs
                    |> List.map feedItem
                    |> table [ class "table is-narrow" ]

        _ ->
            text (toString feeds)


syncButton : Feed -> Html Msg
syncButton feed =
    let
        aClass =
            "button is-small"
                ++ case feed.syncState of
                    RD.NotAsked ->
                        ""

                    RD.Loading ->
                        " is-loading"

                    RD.Success () ->
                        " is-success"

                    RD.Failure e ->
                        " is-danger"
    in
        a [ class aClass, onClick (SyncFeedAsk feed.id) ]
            [ span [ class "icon is-small" ]
                [ i [ class "fa fa-refresh" ] []
                ]
            ]
