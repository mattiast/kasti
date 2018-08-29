module Client exposing (..)

import Json.Decode as D
import Http
import Platform.Cmd as Cmd
import RemoteData as RD
import Helpers as H
import Types exposing (..)
import Client.Types as C


syncFeed : SyncFeedId -> Cmd Msg
syncFeed sfid =
    let
        url =
            case sfid of
                SyncSingle fid ->
                    "/syncfeed/" ++ toString fid

                SyncAll ->
                    "/syncfeed/all"
    in
        Http.post url
            Http.emptyBody
            (D.succeed ())
            |> RD.sendRequest
            |> Cmd.map (SyncFeedReceive sfid)


postProgress : PlayerState -> Cmd Msg
postProgress state =
    C.postProgress (H.encodeProgress state)
        |> RD.sendRequest
        |> Cmd.map (\_ -> PositionsAsk)


getFeeds : Cmd (RD.WebData (List Feed))
getFeeds =
    C.getFeeds
        |> RD.sendRequest
        |> Cmd.map (RD.map (List.map H.makeFeed))


getProgress : Episode -> Cmd (RD.WebData PlayerState)
getProgress ep =
    C.getProgressByEpisodeId ep.id
        |> RD.sendRequest
        |> Cmd.map (RD.map (\prog -> PlayerState ep prog.prPos prog.prDuration))


getPositions : Cmd (RD.WebData (List ProgressInfo))
getPositions =
    C.getProgressAll
        |> RD.sendRequest
        |> Cmd.map (RD.map (List.map H.makeProgressInfo))


getEpisodes : FeedId -> Cmd (RD.WebData (List Episode))
getEpisodes feed_id =
    Http.get ("/episodes/" ++ toString feed_id) (D.list H.decodeEpisode)
        |> RD.sendRequest


getNewEpisodes : Cmd (RD.WebData (List NewEpisode))
getNewEpisodes =
    let
        decodeStuff =
            D.list <|
                D.map H.makeNewEpisode C.decodeNewEpisode
    in
        Http.get ("/episodes/new") decodeStuff
            |> RD.sendRequest


postNewFeed : NewFeed -> Cmd Msg
postNewFeed newFeed =
    Http.post "/feed"
        (Http.jsonBody <| Debug.log "posting" <| H.encodeNewFeed newFeed)
        (D.succeed "")
        |> RD.sendRequest
        |> Cmd.map (RD.map (\_ -> ()) >> NewFeedReceive)
