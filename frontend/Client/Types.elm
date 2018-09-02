module Client.Types exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Time exposing (Posix)
import Iso8601
import Debug exposing (toString)


decodeDate =
    Iso8601.decoder


type alias Episode =
    { epUrl : EpisodeUrl
    , epTitle : String
    , epDate : Posix
    }


decodeEpisode : Decoder Episode
decodeEpisode =
    succeed Episode
        |> required "epUrl" decodeEpisodeUrl
        |> required "epTitle" string
        |> required "epDate" decodeDate


encodeEpisode : Episode -> Json.Encode.Value
encodeEpisode x =
    Json.Encode.object
        [ ( "epUrl", encodeEpisodeUrl x.epUrl )
        , ( "epTitle", Json.Encode.string x.epTitle )
        , ( "epDate", (Json.Encode.string << toString) x.epDate )
        ]


type alias EpisodeUrl =
    { fromEpUrl : String
    }


decodeEpisodeUrl : Decoder EpisodeUrl
decodeEpisodeUrl =
    succeed EpisodeUrl
        |> required "fromEpUrl" string


encodeEpisodeUrl : EpisodeUrl -> Json.Encode.Value
encodeEpisodeUrl x =
    Json.Encode.object
        [ ( "fromEpUrl", Json.Encode.string x.fromEpUrl )
        ]


type alias ProgressMsg =
    { prEpId : Int
    , prPos : Float
    , prDuration : Float
    }


decodeProgressMsg : Decoder ProgressMsg
decodeProgressMsg =
    succeed ProgressMsg
        |> required "prEpId" int
        |> required "prPos" float
        |> required "prDuration" float


encodeProgressMsg : ProgressMsg -> Json.Encode.Value
encodeProgressMsg x =
    Json.Encode.object
        [ ( "prEpId", Json.Encode.int x.prEpId )
        , ( "prPos", Json.Encode.float x.prPos )
        , ( "prDuration", Json.Encode.float x.prDuration )
        ]


type alias FeedInfo =
    { fname : String
    , furl : String
    }


decodeFeedInfo : Decoder FeedInfo
decodeFeedInfo =
    succeed FeedInfo
        |> required "fname" string
        |> required "furl" string


encodeFeedInfo : FeedInfo -> Json.Encode.Value
encodeFeedInfo x =
    Json.Encode.object
        [ ( "fname", Json.Encode.string x.fname )
        , ( "furl", Json.Encode.string x.furl )
        ]


type alias ProgressInfo =
    { pi_ftitle : String
    , pi_epId : Int
    , pi_episode : Episode
    , pi_prog : ProgressMsg
    }


decodeProgressInfo : Decoder ProgressInfo
decodeProgressInfo =
    succeed ProgressInfo
        |> required "pi_ftitle" string
        |> required "pi_epId" int
        |> required "pi_episode" decodeEpisode
        |> required "pi_prog" decodeProgressMsg


encodeProgressInfo : ProgressInfo -> Json.Encode.Value
encodeProgressInfo x =
    Json.Encode.object
        [ ( "pi_ftitle", Json.Encode.string x.pi_ftitle )
        , ( "pi_epId", Json.Encode.int x.pi_epId )
        , ( "pi_episode", encodeEpisode x.pi_episode )
        , ( "pi_prog", encodeProgressMsg x.pi_prog )
        ]


type alias NewEpisode =
    { ne_ftitle : String
    , ne_epId : Int
    , ne_episode : Episode
    }


decodeNewEpisode : Decoder NewEpisode
decodeNewEpisode =
    succeed NewEpisode
        |> required "ne_ftitle" string
        |> required "ne_epId" int
        |> required "ne_episode" decodeEpisode


encodeNewEpisode : NewEpisode -> Json.Encode.Value
encodeNewEpisode x =
    Json.Encode.object
        [ ( "ne_ftitle", Json.Encode.string x.ne_ftitle )
        , ( "ne_epId", Json.Encode.int x.ne_epId )
        , ( "ne_episode", encodeEpisode x.ne_episode )
        ]


type NoContent
    = NoContent


getProgressByEpisodeId : Int -> Http.Request ProgressMsg
getProgressByEpisodeId capture_episodeId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "progress"
                , capture_episodeId |> toString
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeProgressMsg
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getProgressAll : Http.Request (List ProgressInfo)
getProgressAll =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "progress"
                , "all"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeProgressInfo)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postProgress : ProgressMsg -> Http.Request NoContent
postProgress body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "progress"
                ]
        , body =
            Http.jsonBody (encodeProgressMsg body)
        , expect =
            Http.expectStringResponse
                (\r ->
                    if String.isEmpty r.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getFeeds : Http.Request (List ( Int, FeedInfo ))
getFeeds =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "feeds"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 (\x y -> ( x, y )) (index 0 int) (index 1 decodeFeedInfo)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postFeed : FeedInfo -> Http.Request NoContent
postFeed body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "feed"
                ]
        , body =
            Http.jsonBody (encodeFeedInfo body)
        , expect =
            Http.expectStringResponse
                (\r ->
                    if String.isEmpty r.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getEpisodesNew : Http.Request (List NewEpisode)
getEpisodesNew =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "episodes"
                , "new"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeNewEpisode)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getEpisodesByFeedId : Int -> Http.Request (List ( Int, Episode ))
getEpisodesByFeedId capture_feedId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "episodes"
                , capture_feedId |> toString
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 (\x y -> ( x, y )) (index 0 int) (index 1 decodeEpisode)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postSyncfeedAll : Http.Request NoContent
postSyncfeedAll =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "syncfeed"
                , "all"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postSyncfeedByFeedId : Int -> Http.Request NoContent
postSyncfeedByFeedId capture_feedId =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "syncfeed"
                , capture_feedId |> toString
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
