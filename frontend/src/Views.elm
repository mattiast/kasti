module Views exposing (view)

import Browser
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import DateFormat as DF
import Debug exposing (toString)
import Helpers as H
import Html exposing (Html, a, audio, br, div, h4, i, p, small, span, strong, text)
import Html.Attributes as Attr exposing (class, controls, href, id, name, placeholder, src, style, type_)
import Html.Events exposing (onClick, onInput)
import RemoteData as RD
import Time
import Types exposing (..)


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Kasti"
        [ viewMenu model.menuState
        , viewChooser model
        , RD.map viewPlayer model.playerState
            |> RD.withDefault (audio [ id "audio-player", style "display" "none" ] [])
        , case model.view of
            Browse ->
                viewSelector model

            Continue ->
                RD.map (viewPositions << getSortedPositions model.posSortBy) model.positions
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
                    (sortEpisodes newEpisodes)
            ]
        ]


recency : Time.Posix -> Int
recency t =
    0 - Time.posixToMillis t


sortEpisodes : List NewEpisode -> List NewEpisode
sortEpisodes es =
    List.sortBy (\e -> recency e.episode.date) es


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


getSortedPositions : SortMsg -> List ProgressInfo -> List ProgressInfo
getSortedPositions by =
    case by of
        ByFeed ->
            List.sortBy (\pi -> pi.ftitle)

        ByDate ->
            List.sortBy (\pi -> 0 - Time.posixToMillis pi.episode.date)

        ByTime ->
            List.sortBy (\pi -> pi.duration - pi.position)


sortButtons : Html Msg
sortButtons =
    connectedButtons Left
        []
        [ easyButton buttonModifiers [ href "#" ] (SortBy ByFeed) "Feed"
        , easyButton buttonModifiers [ href "#" ] (SortBy ByDate) "Date"
        , easyButton buttonModifiers [ href "#" ] (SortBy ByTime) "Time"
        ]


onePosition : ProgressInfo -> Html Msg
onePosition pi =
    media []
        [ mediaContent [ onClick (EpisodePick pi.episode) ]
            [ p []
                [ strong [] [ text pi.ftitle ]
                , br [] []
                , text pi.episode.title
                ]
            ]
        , mediaRight []
            [ p []
                [ text (H.renderSeconds (pi.duration - pi.position))
                , a [ href "#", onClick (ProgMsg (PostTime (PlayerState pi.episode pi.duration pi.duration False))) ] [ text "Remove" ]
                , br [] []
                , small [] [ text (showTime pi.episode.date) ]
                ]
            , easyProgress { size = Small, color = Info } [] (pi.position / pi.duration)
            ]
        ]


showTime : Time.Posix -> String
showTime time =
    DF.format
        [ DF.yearNumberLastTwo
        , DF.text "/"
        , DF.monthFixed
        , DF.text "/"
        , DF.dayOfMonthFixed
        ]
        Time.utc
        time


viewSelector : Model -> Html Msg
viewSelector model =
    columns columnsModifiers
        []
        [ column narrowColumnModifiers
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
                        [ href "#" ]
                        (SyncFeedAsk SyncAll)
                        ("Sync all feeds" ++ RD.withDefault "" (RD.map toString state.syncAllState))
                    ]
                ]

        hamburgerButton =
            navbarBurger
                state.navbarActive
                [ onClick NavbarToggle, href "#" ]
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
            [ label [] [ text "URL" ]
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
                [ href "#" ]
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
                        [ tableCell [ onClick (AskEpList f.id) ]
                            [ text
                                (f.name
                                    ++ RD.withDefault "" (RD.map toString f.syncState)
                                )
                            ]
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
        [ href "#"
        , onClick (SyncFeedAsk (SyncSingle feed.id))
        ]
        [ span [ class "icon is-small" ]
            [ i [ class "fa fa-refresh" ] [] ]
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
                |> List.sortBy (\ep -> recency ep.date)
                |> List.map episodeRow
            )
        ]


episodeRow : Episode -> Html Msg
episodeRow ep =
    tableRow False
        [ onClick (EpisodePick ep) ]
        [ tableCell [] [ text (showTime ep.date) ]
        , tableCell [] [ text ep.title ]
        ]


viewPlayer : PlayerState -> Html Msg
viewPlayer state =
    div []
        [ h4 [ class "title is-4" ] [ text state.episode.title ]
        , p []
            [ text (showTime state.episode.date) ]
        , br [] []
        , audio
            [ src state.episode.url
            , controls True
            , style "width" "1000px"
            , id "audio-player"
            , Attr.map ProgMsg H.onTimeUpdate
            ]
            []
        , br [] []
        , fields Left
            []
            [ easyButton { buttonModifiers | color = Primary }
                [ href "#" ]
                (ProgMsg (PostTime state))
                "Save"
            , easyButton { buttonModifiers | color = Primary }
                [ href "#" ]
                (ProgMsg (AskTime state.episode))
                "Back"
            , easyButton { buttonModifiers | color = Primary }
                [ href "#" ]
                (ProgMsg (SetDoubleSpeed (not state.doubleSpeed)))
                (if state.doubleSpeed then
                    "2x"

                 else
                    "1x"
                )
            , easyButton { buttonModifiers | color = Primary }
                [ href "#" ]
                (ReceiveProgress (RD.Success { state | time = max (state.time - 20.0) 0.0 }))
                "-20"
            , easyButton { buttonModifiers | color = Primary }
                [ href "#" ]
                (ReceiveProgress (RD.Success { state | time = min (state.time + 20.0) state.duration }))
                "+20"
            ]
        ]
