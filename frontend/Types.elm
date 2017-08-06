module Types exposing (..)

import RemoteData as RD
import Date exposing (Date)


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


type alias Episode =
    { id : Int
    , title : String
    , url : String
    , date : Date
    }


emptyNewFeed : NewFeed
emptyNewFeed =
    NewFeed "" "" RD.NotAsked


type alias State =
    { episode : Episode
    , time : Float
    }


type alias Model =
    { feeds : RD.WebData (List Feed)
    , newFeed : NewFeed
    , episodes : RD.WebData (List Episode)
    , progress : RD.WebData State
    }


type MsgProg
    = TimeUpdate Float
    | PostTime State
    | AskTime Episode


type Msg
    = FeedsReceive (RD.WebData (List Feed))
    | EpisodePick Episode
    | AskEpList Int
    | ReceiveEpList (RD.WebData (List Episode))
    | ReceiveProgress (RD.WebData State)
    | ProgMsg MsgProg
    | Nop
    | SyncFeedAsk FeedId
    | SyncFeedReceive FeedId (RD.WebData ())
    | UpdateNewFeed NewFeed
    | NewFeedPost
    | NewFeedReceive (RD.WebData ())
