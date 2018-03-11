module Views exposing (..)

import Html exposing (Html, div, Attribute, tr, text, h4, p, br, a, i, audio, nav, span, input)
import Html.Events exposing (onClick, on, onInput)
import Html.Attributes exposing (id, style, href, class, src, controls, type_, name, placeholder)
import Types exposing (..)
import RemoteData as RD
import Helpers as H
import Date.Extra as Date
import Date
import Navigation as N
import Bulma.Columns exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Form exposing (field, fields, label, controlInput, controlText, controlInputModifiers, controlButton)
import Bulma.Components
    exposing
        ( navbarBurger
        , navbarLink
        , navbarDropdown
        , navbarDivider
        , navbarItem
        , navbarItemLink
        , navbarMenu
        , navbarStart
        , navbar
        , navbarModifiers
        , navbarBrand
        , tabs
        , tab
        , TabsStyle(..)
        , hoverableNavbarItemDropdown
        )
import Bulma.Elements
    exposing
        ( easyTag
        , table
        , tableModifiers
        , tableBody
        , tableHead
        , tableCell
        , tableCellHead
        , buttonModifiers
        )


tableRow : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
tableRow isSelected =
    tr


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
            tab (model.view == view)
                []
                [ onClick (ClickUrl path) ]
                [ text txt ]
    in
        tabs { style = Boxed, alignment = Left, size = Medium }
            []
            []
            [ item Browse "browse" "Browse"
            , item Continue "continue" "Continue"
            , item New "new" "New"
            ]


viewNewEpisodes : List NewEpisode -> Html Msg
viewNewEpisodes newEpisodes =
    div []
        [ table tableModifiers
            []
            [ tableBody [] <|
                List.map
                    (\ne ->
                        tableRow False
                            [ onClick (EpisodePick ne.episode) ]
                            (List.map (tableCell [] << List.singleton)
                                [ text ne.ftitle
                                , text ne.episode.title
                                ]
                            )
                    )
                    newEpisodes
            ]
        ]


sortEpisodes : List NewEpisode -> List NewEpisode
sortEpisodes es =
    List.sortBy (\e -> -(Date.toTime e.episode.date)) es


viewPositions : List ProgressInfo -> Html Msg
viewPositions prog =
    let
        sec =
            prog
                |> List.map (\pi -> pi.duration - pi.position)
                |> List.sum
                |> H.renderSeconds

        num =
            List.length prog
                |> toString
    in
        div []
            [ text <| sec ++ " (" ++ num ++ " episodes)"
            , table tableModifiers
                []
                [ tableBody [] <|
                    List.map
                        (\pi ->
                            tableRow False
                                [ onClick (EpisodePick pi.episode) ]
                                (List.map (tableCell [] << List.singleton)
                                    [ text pi.ftitle
                                    , text pi.episode.title
                                    , text (H.renderSeconds (pi.duration - pi.position))
                                    ]
                                )
                        )
                        prog
                ]
            ]


myQuarter : Devices (Maybe Width)
myQuarter =
    { mobile = Just Width3
    , tablet = Just Width3
    , desktop = Just Width3
    , widescreen = Just Width3
    , fullHD = Just Width3
    }


viewSelector : Model -> Html Msg
viewSelector model =
    columns columnsModifiers
        []
        [ column { columnModifiers | widths = myQuarter }
            []
            [ feedList model.feeds ]
        , column columnModifiers
            []
            [ model.episodes
                |> RD.map episodeList
                |> RD.withDefault (text (toString model.episodes))
            ]
        ]


viewMenu : MenuState -> Html Msg
viewMenu state =
    let
        ( buttonColor, buttonState ) =
            stateClass state.syncAllState

        syncAllButton =
            navbarItem False
                []
                [ field []
                    [ controlButton { buttonModifiers | color = buttonColor, state = buttonState }
                        []
                        [ onClick (SyncFeedAsk SyncAll) ]
                        [ text "Sync all feeds" ]
                    ]
                ]

        hamburgerButton =
            navbarBurger
                state.navbarActive
                [ onClick NavbarToggle ]
                [ span [] []
                , span [] []
                , span [] []
                ]
    in
        navbar navbarModifiers
            []
            [ navbarBrand []
                hamburgerButton
                [ navbarItem False
                    []
                    [ span [ class "icon is-large" ] [ i [ class "fa fa-google-wallet" ] [] ]
                    , easyTag { size = Large, color = Primary, isLink = False } [] "KASTI"
                    ]
                ]
            , navbarMenu state.navbarActive
                []
                [ navbarStart []
                    [ hoverableNavbarItemDropdown Down
                        []
                        (navbarLink [] [ text "Add feed" ])
                        [ navbarDropdown False
                            Left
                            []
                            ([ syncAllButton
                             , navbarDivider [] []
                             ]
                                ++ viewNewFeedForm state.newFeed
                            )
                        ]
                    ]
                ]
            ]


viewNewFeedForm : NewFeed -> List (Html Msg)
viewNewFeedForm newFeed =
    [ navbarItem False
        []
        [ field []
            [ label [] [ text "Name" ]
            , controlText controlInputModifiers
                []
                [ name "name"
                , placeholder "Feed title"
                , onInput (\v -> UpdateNewFeed { newFeed | name = v })
                ]
                []
            ]
        ]
    , navbarItem False
        []
        [ field []
            [ label [] [ text "Name" ]
            , controlInput controlInputModifiers
                []
                [ type_ "url"
                , name "url"
                , placeholder "http://"
                , onInput (\v -> UpdateNewFeed { newFeed | url = v })
                ]
                []
            ]
        ]
    , let
        ( color, state ) =
            stateClass newFeed.postStatus
      in
        navbarItem False
            []
            [ field []
                [ controlButton { buttonModifiers | color = color, state = state }
                    []
                    [ onClick NewFeedPost ]
                    [ text "Add" ]
                ]
            ]
    ]


stateClass : RD.WebData a -> ( Color, State )
stateClass state =
    case state of
        RD.NotAsked ->
            ( Primary, Blur )

        RD.Loading ->
            ( Primary, Loading )

        RD.Success _ ->
            ( Success, Blur )

        RD.Failure _ ->
            ( Danger, Blur )


feedList : RD.WebData (List Feed) -> Html Msg
feedList feeds =
    case feeds of
        RD.Success fs ->
            let
                feedItem f =
                    tableRow False
                        []
                        [ tableCell [ onClick (AskEpList f.id) ] [ a [] [ text f.name ] ]
                        , tableCell [] [ syncButton f ]
                        ]
            in
                table { tableModifiers | narrow = True }
                    []
                    [ tableBody [] <|
                        List.map feedItem fs
                    ]

        _ ->
            text (toString feeds)


syncButton : Feed -> Html Msg
syncButton feed =
    let
        ( color, state ) =
            stateClass feed.syncState
    in
        controlButton { buttonModifiers | color = color, state = state, size = Small }
            []
            [ onClick (SyncFeedAsk (SyncSingle feed.id)) ]
            [ span [ class "icon is-small" ]
                -- TODO icons too eventually
                [ i [ class "fa fa-refresh" ] []
                ]
            ]


episodeList : List Episode -> Html Msg
episodeList eps =
    table { tableModifiers | narrow = True }
        []
        [ tableHead []
            [ tableRow False
                []
                [ tableCellHead [] [ text "Date" ]
                , tableCellHead [] [ text "Title" ]
                ]
            ]
        , tableBody [] <|
            (eps
                |> List.sortWith (\x y -> Date.compare y.date x.date)
                |> List.map episodeRow
            )
        ]


episodeRow : Episode -> Html Msg
episodeRow ep =
    tableRow False
        [ onClick (EpisodePick ep) ]
        [ tableCell [] [ text (Date.toFormattedString "y/M/d" ep.date) ]
        , tableCell [] [ text ep.title ]
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
        , fields Left
            []
            [ controlButton { buttonModifiers | color = Primary }
                []
                [ onClick (PostTime state) ]
                [ text "Save position" ]
            , controlButton { buttonModifiers | color = Primary }
                []
                [ onClick (AskTime state.episode) ]
                [ text "Get position" ]
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
