module Views exposing (..)

import Html exposing (Html, div, text, h4, p, br, a, i, audio, span, strong, small)
import Html.Events exposing (onClick, on, onInput)
import Html.Attributes exposing (id, style, href, class, src, controls, type_, name, placeholder)
import Types exposing (..)
import RemoteData as RD
import Helpers as H
import Browser.Navigation as N
import Browser
import Bulma.Columns exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Form exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Debug exposing (toString)
import Iso8601


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Kasti"
        [ viewMenu model.menuState
        , viewChooser model
        , RD.map viewPlayer model.playerState
            |> RD.withDefault (audio [ id "audio-player", style "display" "none" ] [])
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
        item viu path txt =
            tab (model.view == viu)
                []
                [ href path ]
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
    es


positionAggregateInfo : List ProgressInfo -> Html Msg
positionAggregateInfo prog =
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
        p [] [ text <| "Total time left: " ++ sec ++ " (" ++ num ++ " episodes)" ]


viewPositions : List ProgressInfo -> Html Msg
viewPositions prog =
    columns { columnsModifiers | centered = True }
        []
        [ column columnModifiers
            []
            ([ positionAggregateInfo prog
             , sortButtons
             ]
                ++ List.map onePosition prog
            )
        ]


sortButtons : Html Msg
sortButtons =
    connectedButtons Left
        []
        [ easyButton buttonModifiers [] (SortBy ByFeed) "Feed"
        , easyButton buttonModifiers [] (SortBy ByDate) "Date"
        , easyButton buttonModifiers [] (SortBy ByTime) "Time"
        ]


onePosition : ProgressInfo -> Html Msg
onePosition pi =
    media [ onClick (EpisodePick pi.episode) ]
        [ mediaContent []
            [ p []
                [ strong [] [ text pi.ftitle ]
                , br [] []
                , text pi.episode.title
                ]
            ]
        , mediaRight []
            [ p []
                [ text (H.renderSeconds (pi.duration - pi.position))
                , br [] []
                , small [] [ text (Iso8601.fromTime pi.episode.date) ]
                ]
            , easyProgress { size = Small, color = Info } [] (pi.position / pi.duration)
            ]
        ]


devices : Width -> Devices (Maybe Width)
devices w =
    { mobile = Just w
    , tablet = Just w
    , desktop = Just w
    , widescreen = Just w
    , fullHD = Just w
    }


viewSelector : Model -> Html Msg
viewSelector model =
    columns columnsModifiers
        []
        [ column { columnModifiers | widths = devices Width3 }
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
                    [ easyButton { buttonModifiers | color = buttonColor, state = buttonState }
                        []
                        (SyncFeedAsk SyncAll)
                        "Sync all feeds"
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
                [ easyButton { buttonModifiers | color = color, state = state }
                    []
                    NewFeedPost
                    "Add"
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
                        [ tableCell [ onClick (AskEpList f.id) ] [ text f.name ]
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
                |> List.map episodeRow
            )
        ]


episodeRow : Episode -> Html Msg
episodeRow ep =
    tableRow False
        [ onClick (EpisodePick ep) ]
        [ tableCell [] [ text (Iso8601.fromTime ep.date) ]
        , tableCell [] [ text ep.title ]
        ]


viewPlayer : PlayerState -> Html MsgProg
viewPlayer state =
    div []
        [ h4 [ class "title is-4" ] [ text state.episode.title ]
        , p []
            [ text (Iso8601.fromTime state.episode.date) ]
        , br [] []
        , audio
            [ src state.episode.url
            , controls True
            , style "width" "1000px"
            , id "audio-player"
            , H.onTimeUpdate
            ]
            []
        , br [] []
        , fields Left
            []
            [ easyButton { buttonModifiers | color = Primary }
                []
                (PostTime state)
                "Save position"
            , easyButton { buttonModifiers | color = Primary }
                []
                (AskTime state.episode)
                "Get position"
            , easyButton { buttonModifiers | color = Primary }
                []
                (SetPlaybackRate 2.0)
                "2x speed"
            ]
        ]
