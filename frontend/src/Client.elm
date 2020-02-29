module Client exposing (getEpisodes, getFeeds, getNewEpisodes, getPositions, getProgress, postNewFeed, postProgress, syncFeed)

import Client.Types as C
import Helpers as H
import Http
import Json.Decode as D
import Platform.Cmd as Cmd
import RemoteData as RD
import Types exposing (..)


syncFeed : SyncFeedId -> Cmd Msg
syncFeed sfid =
    let
        cont =
            Result.map (\_ -> ()) >> RD.fromResult >> SyncFeedReceive sfid
    in
    case sfid of
        SyncSingle fid ->
            C.postSyncfeedByFeedId fid cont

        SyncAll ->
            C.postSyncfeedAll cont


postProgress : PlayerState -> Cmd Msg
postProgress state =
    C.postProgress (H.encodeProgress state) (\_ -> PositionsAsk)


getFeeds : Cmd (RD.WebData (List Feed))
getFeeds =
    C.getFeeds
        (RD.fromResult >> RD.map (List.map H.makeFeed))


getProgress : Episode -> Cmd (RD.WebData PlayerState)
getProgress ep =
    C.getProgressByEpisodeId
        ep.id
        (RD.fromResult >> RD.map (\prog -> PlayerState ep prog.prPos prog.prDuration False))


getPositions : Cmd (RD.WebData (List ProgressInfo))
getPositions =
    C.getProgressAll
        (RD.fromResult >> RD.map (List.map H.makeProgressInfo))


getEpisodes : FeedId -> Cmd (RD.WebData (List Episode))
getEpisodes feed_id =
    C.getEpisodesByFeedId feed_id (RD.fromResult >> RD.map (List.map H.makeEpisode))


getNewEpisodes : Cmd (RD.WebData (List NewEpisode))
getNewEpisodes =
    C.getEpisodesNew
        (RD.fromResult >> RD.map (List.map H.makeNewEpisode))


postNewFeed : NewFeed -> Cmd Msg
postNewFeed newFeed =
    C.postFeed (H.encodeNewFeed newFeed) (RD.fromResult >> RD.map (\_ -> ()) >> NewFeedReceive)
