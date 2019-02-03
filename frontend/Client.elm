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
        req =
            case sfid of
                SyncSingle fid ->
                    C.postSyncfeedByFeedId fid

                SyncAll ->
                    C.postSyncfeedAll
    in
        req
            |> RD.sendRequest
            |> Cmd.map (RD.map (\_ -> ()) >> SyncFeedReceive sfid)


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
        |> Cmd.map (RD.map (\prog -> PlayerState ep prog.prPos prog.prDuration False))


getPositions : Cmd (RD.WebData (List ProgressInfo))
getPositions =
    C.getProgressAll
        |> RD.sendRequest
        |> Cmd.map (RD.map (List.map H.makeProgressInfo))


getEpisodes : FeedId -> Cmd (RD.WebData (List Episode))
getEpisodes feed_id =
    C.getEpisodesByFeedId feed_id
        |> RD.sendRequest
        |> Cmd.map (RD.map (List.map (\( id, ep ) -> H.makeEpisode id ep)))


getNewEpisodes : Cmd (RD.WebData (List NewEpisode))
getNewEpisodes =
    C.getEpisodesNew
        |> RD.sendRequest
        |> Cmd.map (RD.map (List.map H.makeNewEpisode))


postNewFeed : NewFeed -> Cmd Msg
postNewFeed newFeed =
    C.postFeed (H.encodeNewFeed newFeed)
        |> RD.sendRequest
        |> Cmd.map (RD.map (\_ -> ()) >> NewFeedReceive)
