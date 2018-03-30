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
    { ftitle = cp.ftitle
    , episode = makeEpisode cp.epId cp.episode
    , position = cp.msg.prPos
    , duration = cp.msg.prDuration
    }


encodeProgress : PlayerState -> JE.Value
encodeProgress state =
    C.encodeProgressMsg
        { prEpId = state.episode.id
        , prPos = state.time
        , prDuration = state.duration
        }


encodeNewFeed : NewFeed -> JE.Value
encodeNewFeed newFeed =
    C.encodeFeedInfo
        { fname = newFeed.name
        , furl = newFeed.url
        }


decodeFeed : J.Decoder Feed
decodeFeed =
    J.map2 makeFeed
        (J.index 0 J.int)
        (J.index 1 <| C.decodeFeedInfo)


makeFeed : Int -> C.FeedInfo -> Feed
makeFeed id fi =
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
