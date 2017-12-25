module Views exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on, onInput)
import Html.Attributes exposing (id, style, href, class, src, controls, type_, name, placeholder)
import Types exposing (..)
import RemoteData as RD
import Helpers as H
import Date.Extra as Date
import Date
import Navigation as N


view : Model -> Html Msg
view model =
    div []
        [ viewMenu model.menuState
        , viewChooser model
        , RD.map viewPlayer model.playerState
            |> RD.withDefault (audio [ id "audio-player", style [ ( "display", "none" ) ] ] [])
            |> Html.map ProgMsg
        , case model.view of
            Browse ->
                viewSelector model

            Continue ->
                RD.map viewPositions model.positions
                    |> RD.withDefault (text (toString model.positions))

            New ->
                RD.map viewNewEpisodes model.newEpisodes
                    |> RD.withDefault (text (toString model.newEpisodes))
        ]


viewChooser : Model -> Html Msg
viewChooser model =
    let
        item view path txt =
            li
                (if model.view == view then
                    [ class "is-active" ]
                 else
                    []
                )
                [ a [ onClick (ClickUrl path) ] [ text txt ] ]
    in
        div [ class "tabs is-boxed is-medium" ]
            [ ul []
                [ item Browse "browse" "Browse"
                , item Continue "continue" "Continue"
                , item New "new" "New"
                ]
            ]


viewNewEpisodes : List NewEpisode -> Html Msg
viewNewEpisodes newEpisodes =
    div []
        [ table [ class "table" ] <|
            List.map
                (\ne ->
                    tr [ onClick (EpisodePick ne.episode) ]
                        (List.map (td [] << List.singleton)
                            [ text ne.ftitle
                            , text ne.episode.title
                            ]
                        )
                )
                newEpisodes
        ]


sortEpisodes : List NewEpisode -> List NewEpisode
sortEpisodes es =
    List.sortBy (\e -> -(Date.toTime e.episode.date)) es


viewPositions : List ProgressInfo -> Html Msg
viewPositions prog =
    div []
        [ prog
            |> List.map (\pi -> pi.duration - pi.position)
            |> List.sum
            |> H.renderSeconds
            |> text
        , table [ class "table" ] <|
            List.map
                (\pi ->
                    tr [ onClick (EpisodePick pi.episode) ]
                        (List.map (td [] << List.singleton)
                            [ text pi.ftitle
                            , text pi.episode.title
                            , text (H.renderSeconds (pi.duration - pi.position))
                            ]
                        )
                )
                prog
        ]


viewSelector : Model -> Html Msg
viewSelector model =
    div [ class "columns" ]
        [ div [ class "column is-one-quarter" ]
            [ feedList model.feeds
            ]
        , div [ class "column" ]
            [ model.episodes
                |> RD.map episodeList
                |> RD.withDefault (text (toString model.episodes))
            ]
        ]


viewMenu : MenuState -> Html Msg
viewMenu state =
    let
        activeString =
            if state.navbarActive then
                " is-active"
            else
                ""

        syncAllButton =
            div [ class "navbar-item" ]
                [ div [ class "field" ]
                    [ div [ class "control" ]
                        [ button [ class ("button " ++ syncStateClass state.syncAllState), onClick (SyncFeedAsk SyncAll) ]
                            [ text "Sync all feeds" ]
                        ]
                    ]
                ]

        hamburgerButton =
            button
                [ class ("button navbar-burger" ++ activeString)
                , onClick NavbarToggle
                ]
                [ span [] []
                , span [] []
                , span [] []
                ]
    in
        nav [ class "navbar" ]
            [ div [ class "navbar-brand" ]
                [ div [ class "navbar-item" ]
                    [ span [ class "icon is-large" ] [ i [ class "fa fa-google-wallet" ] [] ]
                    , span [ class "tag is-primary is-large" ] [ text "KASTI" ]
                    ]
                , hamburgerButton
                ]
            , div [ class ("navbar-menu" ++ activeString) ]
                [ div [ class "navbar-start" ]
                    [ div [ class "navbar-item has-dropdown is-hoverable" ]
                        [ a [ class "navbar-link is-active" ] [ text "Add feed" ]
                        , div [ class "navbar-dropdown" ]
                            ([ syncAllButton
                             ]
                                ++ viewNewFeedForm state.newFeed
                            )
                        ]
                    ]
                ]
            ]


viewNewFeedForm : NewFeed -> List (Html Msg)
viewNewFeedForm newFeed =
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


addFeedButtonClass : NewFeed -> String
addFeedButtonClass newFeed =
    "button " ++ syncStateClass newFeed.postStatus


syncStateClass : RD.WebData a -> String
syncStateClass state =
    case state of
        RD.NotAsked ->
            "is-primary"

        RD.Loading ->
            "is-primary is-loading"

        RD.Success _ ->
            "is-success"

        RD.Failure _ ->
            "is-danger"


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
        a [ class aClass, onClick (SyncFeedAsk (SyncSingle feed.id)) ]
            [ span [ class "icon is-small" ]
                [ i [ class "fa fa-refresh" ] []
                ]
            ]


episodeList : List Episode -> Html Msg
episodeList eps =
    eps
        |> List.sortWith (\x y -> Date.compare y.date x.date)
        |> List.map episodeRow
        |> (\rows -> [ tr [] [ th [] [ text "Date" ], th [] [ text "Title" ] ] ] ++ rows)
        |> table [ class "table is-narrow" ]


episodeRow : Episode -> Html Msg
episodeRow ep =
    tr [ onClick (EpisodePick ep) ]
        [ td [] [ text (Date.toFormattedString "y/M/d" ep.date) ]
        , td [] [ text ep.title ]
        ]


viewPlayer : PlayerState -> Html MsgProg
viewPlayer state =
    div []
        [ showState state
        , br [] []
        , audio
            [ src state.episode.url
            , controls True
            , style [ ( "width", "1000px" ) ]
            , id "audio-player"
            , H.onTimeUpdate
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


showState : PlayerState -> Html msg
showState state =
    div []
        [ h4 [ class "title is-4" ] [ text state.episode.title ]
        , p []
            [ text (Date.toFormattedString "y/M/d" state.episode.date)
            ]
        , a [ href state.episode.url ] [ text "[mp3 link]" ]
        ]
