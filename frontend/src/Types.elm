module Types exposing (Episode, EpisodeId, Feed, FeedId, MenuState, Model, Msg(..), MsgProg(..), NewEpisode, NewFeed, PlayerState, ProgressInfo, SortMsg(..), SyncFeedId(..), WhichView(..), emptyNewFeed)

import Browser
import Browser.Navigation as N
import RemoteData as RD
import Time exposing (Posix)
import Url as U


type alias FeedId =
    Int


type alias EpisodeId =
    Int


type alias Feed =
    { id : FeedId
    , name : String
    , url : String
    , syncState : RD.WebData Int
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
    , date : Posix
    }


emptyNewFeed : NewFeed
emptyNewFeed =
    NewFeed "" "" RD.NotAsked


type alias PlayerState =
    { episode : Episode
    , time : Float
    , duration : Float
    , doubleSpeed : Bool
    }


type alias Model =
    { feeds : RD.WebData (List Feed)
    , episodes : RD.WebData (List Episode)
    , playerState : RD.WebData PlayerState
    , menuState : MenuState
    , view : WhichView
    , navKey : N.Key
    , positions : RD.WebData (List ProgressInfo)
    , posSortBy : SortMsg
    , newEpisodes : RD.WebData (List NewEpisode)
    }


type alias MenuState =
    { newFeed : NewFeed
    , syncAllState : RD.WebData Int
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
    | SetDoubleSpeed Bool


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
    | SyncFeedReceive SyncFeedId (RD.WebData Int)
    | UpdateNewFeed NewFeed
    | NewFeedPost
    | NewFeedReceive (RD.WebData ())
    | PositionsReceive (RD.WebData (List ProgressInfo))
    | PositionsAsk
    | UrlChange U.Url
    | ClickUrl Browser.UrlRequest
    | NewEpisodesReceive (RD.WebData (List NewEpisode))
    | NavbarToggle
    | SortBy SortMsg


type SortMsg
    = ByFeed
    | ByDate
    | ByTime
