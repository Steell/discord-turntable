{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Types where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.Function
import           Data.Monoid
import           Data.Sequence
import           Data.Set

import           Network.Discord.Types

type Track = String --TODO: Url, plus maybe more info
type Queue = Seq Track

data PlayInfo = PlayInfo { _track :: Track
                         , _dj    :: User
                         }
    deriving (Show)
makeClassy ''PlayInfo

data CommandEnv = CommandEnv { _nowPlaying :: PlayInfo
                             , _cmdUser :: User
                             , _cmdChannel :: Channel
                             }
makeClassy ''CommandEnv

data BotState = BotState { _botSkipSet :: !(Set Snowflake)
                         , _botTextChannel :: !Snowflake
                         }
    deriving (Show)
makeClassy ''BotState

newtype CommandM a = CommandM { unwrapCmdM :: ReaderT CommandEnv (StateT BotState IO) a
                              }
    deriving (Functor, Alternative, Applicative, Monad, MonadReader CommandEnv, MonadState BotState, MonadIO, MonadPlus)

type Url = String

type CommandName = String

type Amount = Int {-Absolute Int
            | Relative Int
  deriving (Read, Show)-}

type SongQuery = String
               {-SongLink Url
               | TextQuery String-}

{-
data CommandInfo = CommandInfo { _cmdName        :: CommandName
                               , _cmdDesc        :: String
                               , _cmdPermissions :: Set Permission
                               -- , _cmd            :: CommandM ()
                               }
makeClassy ''CommandInfo
-}
