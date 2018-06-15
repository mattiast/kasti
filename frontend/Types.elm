module Types exposing (..)

import RemoteData as RD
import Date exposing (Date)
import Navigation as N


type alias FeedId =
    Int


type alias EpisodeId =
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
    { id : EpisodeId
    , title : String
    , url : String
    , date : Date
    }


emptyNewFeed : NewFeed
emptyNewFeed =
    NewFeed "" "" RD.NotAsked


type alias PlayerState =
    { episode : Episode
    , time : Float
    , duration : Float
    }


type alias Model =
    { feeds : RD.WebData (List Feed)
    , episodes : RD.WebData (List Episode)
    , playerState : RD.WebData PlayerState
    , menuState : MenuState
    , view : WhichView
    , positions : RD.WebData (List ProgressInfo)
    , newEpisodes : RD.WebData (List NewEpisode)
    }


type alias MenuState =
    { newFeed : NewFeed
    , syncAllState : RD.WebData ()
    , navbarActive : Bool
    }


type alias NewEpisode =
    { ftitle : String
    , episode : Episode
    }


type alias ProgressInfo =
    { ftitle : String
    , episode : Episode
    , position : Float
    , duration : Float
    }


type MsgProg
    = TimeUpdate Float Float
    | PostTime PlayerState
    | AskTime Episode
    | SetPlaybackRate Float


type WhichView
    = Browse
    | Continue
    | New


type SyncFeedId
    = SyncSingle FeedId
    | SyncAll


type Msg
    = FeedsReceive (RD.WebData (List Feed))
    | EpisodePick Episode
    | AskEpList Int
    | ReceiveEpList (RD.WebData (List Episode))
    | ReceiveProgress (RD.WebData PlayerState)
    | ProgMsg MsgProg
    | Nop
    | SyncFeedAsk SyncFeedId
    | SyncFeedReceive SyncFeedId (RD.WebData ())
    | UpdateNewFeed NewFeed
    | NewFeedPost
    | NewFeedReceive (RD.WebData ())
    | PositionsReceive (RD.WebData (List ProgressInfo))
    | PositionsAsk
    | UrlChange N.Location
    | ClickUrl String
    | NewEpisodesReceive (RD.WebData (List NewEpisode))
    | NavbarToggle
    | SortBy SortMsg


type SortMsg
    = ByFeed
    | ByDate
    | ByTime
