module Types exposing (..)

import RemoteData as RD
import Episode as E
import Play as P


type alias FeedId =
    Int


type alias Feed =
    { id : FeedId
    , name : String
    , url : String
    , syncState : RD.WebData ()
    }


type alias NewFeed =
    { name : String
    , url : String
    , postStatus : RD.WebData ()
    }


emptyNewFeed : NewFeed
emptyNewFeed =
    NewFeed "" "" RD.NotAsked


type alias Model =
    { feeds : RD.WebData (List Feed)
    , newFeed : NewFeed
    , episodes : RD.WebData (List E.Episode)
    , progress : RD.WebData P.State
    }


type Msg
    = FeedsReceive (RD.WebData (List Feed))
    | EpMsg E.Msg
    | AskEpList Int
    | ReceiveEpList (RD.WebData (List E.Episode))
    | ReceiveProgress (RD.WebData P.State)
    | ProgMsg P.Msg
    | Nop
    | SyncFeedAsk FeedId
    | SyncFeedReceive FeedId (RD.WebData ())
    | UpdateNewFeed NewFeed
    | NewFeedPost
    | NewFeedReceive (RD.WebData ())
