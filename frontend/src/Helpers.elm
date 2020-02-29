module Helpers exposing (encodeNewFeed, encodeProgress, makeEpisode, makeFeed, makeNewEpisode, makeProgressInfo, modifyFeedAtId, modifyMenuState, modifyPositions, onTimeUpdate, renderSeconds, targetCurrentTime, targetDuration)

import Client.Types as C
import DateFormat as DF
import Debug exposing (toString)
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as J
import Json.Encode as JE
import RemoteData as RD
import Time
import Types exposing (..)


makeEpisode : C.EStuff -> Episode
makeEpisode (C.EStuff id ce) =
    { title = ce.epTitle
    , url = ce.epUrl
    , date = ce.epDate
    , id = id
    }


makeProgressInfo : C.ProgressInfo -> ProgressInfo
makeProgressInfo cp =
    { ftitle = cp.pi_ftitle
    , episode = makeEpisode (C.EStuff cp.pi_epId cp.pi_episode)
    , position = cp.pi_prog.prPos
    , duration = cp.pi_prog.prDuration
    }


makeNewEpisode : C.NewEpisode -> NewEpisode
makeNewEpisode cn =
    { ftitle = cn.ne_ftitle
    , episode = makeEpisode (C.EStuff cn.ne_epId cn.ne_episode)
    }


encodeProgress : PlayerState -> C.ProgressMsg
encodeProgress state =
    { prEpId = state.episode.id
    , prPos = state.time
    , prDuration = state.duration
    }


encodeNewFeed : NewFeed -> C.FeedInfo
encodeNewFeed newFeed =
    { fname = newFeed.name
    , furl = newFeed.url
    }


makeFeed : C.FStuff -> Feed
makeFeed (C.FStuff id fi) =
    { id = id
    , name = fi.fname
    , url = fi.furl
    , syncState = RD.NotAsked
    }


onTimeUpdate : Attribute MsgProg
onTimeUpdate =
    on "timeupdate" (J.map2 TimeUpdate targetCurrentTime targetDuration)


targetCurrentTime : J.Decoder Float
targetCurrentTime =
    J.at [ "target", "currentTime" ] J.float


targetDuration : J.Decoder Float
targetDuration =
    J.at [ "target", "duration" ] J.float


modifyFeedAtId : FeedId -> (Feed -> Feed) -> Model -> Model
modifyFeedAtId fid upd model =
    let
        updAt : Feed -> Feed
        updAt feed =
            if feed.id == fid then
                upd feed

            else
                feed
    in
    { model | feeds = RD.map (List.map updAt) model.feeds }


modifyMenuState : (MenuState -> MenuState) -> Model -> Model
modifyMenuState upd model =
    { model | menuState = upd model.menuState }


modifyPositions : (List ProgressInfo -> List ProgressInfo) -> (Model -> Model)
modifyPositions f model =
    { model | positions = RD.map f model.positions }


renderSeconds : Float -> String
renderSeconds sec =
    let
        time =
            Time.millisToPosix (round (sec * 1000))
    in
    DF.format
        [ DF.hourMilitaryNumber
        , DF.text ":"
        , DF.minuteFixed
        , DF.text ":"
        , DF.secondFixed
        ]
        Time.utc
        time
