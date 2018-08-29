module Helpers exposing (..)

import Date exposing (Date)
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as J
import Json.Encode as JE
import RemoteData as RD
import Types exposing (..)
import Time.DateTime as T
import Formatting as F
import Formatting exposing ((<>))
import Client.Types as C


decodeEpisode : J.Decoder Episode
decodeEpisode =
    J.map2 makeEpisode
        (J.index 0 J.int)
        (J.index 1 C.decodeEpisode)


makeEpisode : Int -> C.Episode -> Episode
makeEpisode id ce =
    { title = ce.epTitle
    , url = ce.epUrl.fromEpUrl
    , date = ce.epDate
    , id = id
    }


makeProgressInfo : C.ProgressInfo -> ProgressInfo
makeProgressInfo cp =
    { ftitle = cp.pi_ftitle
    , episode = makeEpisode cp.pi_epId cp.pi_episode
    , position = cp.pi_prog.prPos
    , duration = cp.pi_prog.prDuration
    }


makeNewEpisode : C.NewEpisode -> NewEpisode
makeNewEpisode cn =
    { ftitle = cn.ne_ftitle
    , episode = makeEpisode cn.ne_epId cn.ne_episode
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


makeFeed : ( Int, C.FeedInfo ) -> Feed
makeFeed ( id, fi ) =
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
        z =
            T.dateTime T.zero

        x =
            T.addSeconds (floor sec) z

        pad2 =
            F.padLeft 2 '0' F.int

        fmt =
            F.int <> F.s ":" <> pad2 <> F.s ":" <> pad2
    in
        F.print fmt (T.hour x) (T.minute x) (T.second x)
