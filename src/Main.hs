{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Conduit

import           Control.Applicative ( Alternative, optional )
import           Control.Concurrent.Event ( Event )
import qualified Control.Concurrent.Event as Event
import           Control.Concurrent.STM ( STM, TMVar, TVar, atomically
                                        , newEmptyTMVarIO, newTVarIO, putTMVar
                                        , readTMVar, readTVar, readTVarIO
                                        , retry, swapTMVar, writeTVar )
import           Control.Lens hiding ( (:<), (|>) )
import           Control.Lens.At ( at )
import           Control.Monad ( MonadPlus, filterM, forever )
import           Control.Monad.Extra ( mapMaybeM )
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans

import           Data.Acid ( AcidState )
import qualified Data.Acid as Acid
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import           Data.Conduit.Combinators ( omapME )
import           Data.Conduit.Network.UDP ( sinkSocket )
import           Data.Conduit.Process ( CreateProcess, sourceProcessWithStreams )
import           Data.Foldable ( fold )
import           Data.Function ( on )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.List as List
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe, listToMaybe, mapMaybe )
import           Data.Monoid ( (<>) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Proxy as Proxy
import qualified Data.SafeCopy as SC
import           Data.Sequence ( (|>), Seq, ViewL(..) )
import qualified Data.Sequence as Seq
import           Data.Sequence.Lens ( viewL )
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable ( Typeable )
import           Data.Word ( Word8 )

import           GHC.TypeLits ( KnownSymbol, Symbol )

import           Network.Socket ( Socket )

import qualified Options.Applicative as OP

import           System.IO ( BufferMode(NoBuffering), hSetBuffering, stdout )
import           System.IO.Unsafe ( unsafePerformIO )

import           Network.Discord hiding ( Event, Text )

import           Types

data BotDB = BotDB { _userQueues   :: Map Snowflake Queue
                   , _playerVolume :: Word8
                   , _blacklist    :: Set Track
                   }
    deriving (Typeable)
makeClassy ''BotDB

findM :: (Foldable t, Monad m) => (a -> m (Maybe b)) -> t a -> m (Maybe b)
findM f = foldr fun (return Nothing)
  where fun a b = maybe b (return . Just) =<< f a

popNextTrack :: Seq Snowflake -> Acid.Update BotDB (Maybe (Snowflake, Track))
popNextTrack uids = do
  qs <- use userQueues
  findM (hasTrack qs) uids
  where
    hasTrack qs uid = case qs^.at uid of
      Just q -> case q ^. viewL of
        (t :< ts) -> do
          userQueues.at uid ?= (ts |> t)
          return $ Just (uid, t)
        _ -> return Nothing
      Nothing -> return Nothing

addTrack :: Snowflake -> Track -> Acid.Update BotDB ()
addTrack uid t =
  userQueues.at uid %= Just . maybe (Seq.singleton t) (|> t)

getUserQueue :: Snowflake -> Acid.Query BotDB (Seq Track)
getUserQueue uid = fromMaybe Seq.empty <$> view (userQueues.at uid)

setVolume :: Maybe Word8 -> Acid.Update BotDB Word8
setVolume Nothing = use playerVolume
setVolume (Just v) = playerVolume <.= v

SC.deriveSafeCopy 0 'SC.base ''Snowflake
SC.deriveSafeCopy 0 'SC.base ''BotDB
Acid.makeAcidic ''BotDB [ 'popNextTrack, 'addTrack, 'getUserQueue, 'setVolume ]

nowPlayingRef :: TVar (Maybe PlayInfo)
nowPlayingRef = unsafePerformIO $ newTVarIO Nothing

botStateRef :: TMVar (AcidState BotDB)
botStateRef = unsafePerformIO newEmptyTMVarIO

botStateRefAcidless :: TMVar BotDB
botStateRefAcidless = unsafePerformIO newEmptyTMVarIO

data PlayState = Playing | Paused | Stopped

playStateTVar :: TVar PlayState
playStateTVar = unsafePerformIO $ newTVarIO Stopped

volumeRef :: IORef Word8
volumeRef = unsafePerformIO $ newIORef 30

voiceSocketRef :: TMVar Socket
voiceSocketRef = unsafePerformIO $ newEmptyTMVarIO

djOrderRef :: TMVar (Seq Snowflake)
djOrderRef = unsafePerformIO $ newEmptyTMVarIO

-- player thread
play :: AcidState BotDB -> Event -> IO ()
play botState plEvt =
  forever $ join . atomically $ do
    state <- readTVar playStateTVar
    case state of
        -- wait until we're playing again
        Stopped -> retry
        _ -> return $ findNextTrack >>= \case
          Just t -> void $ playTrack t
          Nothing -> Event.wait plEvt
  where
    findNextTrack :: IO (Maybe Track)
    findNextTrack = do
      uids <- atomically (readTMVar djOrderRef)
      mt <- Acid.update botState $ PopNextTrack uids
      case mt of
        Just (uid, t) -> do
          atomically . void . swapTMVar djOrderRef $ updateDjOrder uid uids
          return $ Just t
        Nothing -> return Nothing

    updateDjOrder uid uids =
      let (skipped, rem) = Seq.breakl (== uid) uids in
        rem <> skipped

    playTrack t =
      sourceProcessWithStreams (ytdl t)
                               (return ()) -- stdin
                               (playerControlC
                                =$= applyVolumeC
                                =$= terminateWithSilenceC
                                =$= attachSocketC
                                =$= sinkVoiceSocket)
                               (return ()) --TODO: stderr

    -- TODO: handle skips
    playerControlC :: Conduit ByteString IO ByteString
    playerControlC = join . lift . atomically $
        readTVar playStateTVar >>= \case
          Playing -> return $
              await >>= \case
                (Just x) -> yield x >> playerControlC
                Nothing -> return ()
          Paused -> retry
          Stopped -> return $ return ()

    terminateWithSilenceC :: Monad m => Conduit ByteString m ByteString
    terminateWithSilenceC = do
      await >>= \case Nothing -> yield silence
                      (Just x) -> yield x >> terminateWithSilenceC

    silence = BS.pack [ 0xF8, 0xFF, 0xFE ]

    applyVolumeC :: Conduit ByteString IO ByteString
    applyVolumeC = omapME $ \b -> (b *) <$> readIORef volumeRef

    attachSocketC :: Conduit ByteString IO (ByteString, Socket)
    attachSocketC = mapMC $ \a -> do
       s <- atomically $ readTMVar voiceSocketRef
       return (a, s)

    sinkVoiceSocket :: Consumer (ByteString, Socket) IO ()
    sinkVoiceSocket = awaitForever $ \(a, s) -> yield a =$= sinkSocket s

ytdl :: Track -> CreateProcess
ytdl t = undefined --TODO

cmdParser :: OP.Parser (CommandM ())
cmdParser = OP.subparser $
    mconcat [ OP.command "queue" (OP.info enqueueCmdP mempty)
            , OP.command "np" (OP.info nowPlayingCmdP mempty)
            , OP.command "skip" (OP.info voteSkipCmdP mempty)
            , OP.command "summon" (OP.info summonCmdP mempty)
            , OP.command "volume" (OP.info volumeCmdP mempty)
            ]
  where
    --helpCmdP = helpCmd <$> (optional $ OP.strArgument (OP.metavar "COMMAND"))
    enqueueCmdP = enqueueCmd <$> optional (OP.strArgument (OP.metavar "TRACK"))
    nowPlayingCmdP = pure nowPlayingCmd
    voteSkipCmdP = pure voteSkipCmd
    summonCmdP = pure summonCmd
    volumeCmdP = setVolumeCmd <$> optional (OP.argument OP.auto
                                                        (OP.metavar "VOLUME"))

enqueueCmd Nothing = showQueue
enqueueCmd (Just q) = do
  let track = resolveQuery q
  botState <- liftIO . atomically $ readTMVar botStateRef
  uid <- view $ cmdUser.to userId
  liftIO . Acid.update botState $ AddTrack uid track
  printToChat $ "Added to queue: " <> show track

showQueue = do
  uid <- view $ cmdUser.to userId
  botState <- liftIO . atomically $ readTMVar botStateRef
  q <- liftIO . Acid.query botState $ GetUserQueue uid
  printToChat $ pretty q
    where
      pretty = fold . Seq.intersperse "\n" . fmap show

nowPlayingCmd = printToChat =<< view (nowPlaying . to show)

voteSkipCmd = do
    u <- view $ cmdUser.to userId
    dj <- view $ nowPlaying.dj.to userId
    if u == dj
      then nextSong
      else do
        botSkipSet %= Set.insert u
        numSkips <- use $ botSkipSet.to Set.size
        threshold <- calcSkipThreshold
        when (numSkips >= threshold) nextSong
  where
    calcSkipThreshold = undefined --TODO: read from configuration?

summonCmd = moveToVoiceChannel =<< userVoiceChannel =<< view cmdUser

userVoiceChannel :: User -> CommandM Channel
userVoiceChannel uid = undefined --TODO: implement voice support

setVolumeCmd mv = do
    botState <- liftIO . atomically $ readTMVar botStateRef
    liftIO $ do
      v <- Acid.update botState $ SetVolume mv
      writeIORef volumeRef v -- TODO: FRP use-case?

adjustBy :: Amount -> Int -> Int
{- TODO: re-enable to support relative volume adjustment via +<int> -<int>
adjustBy (Absolute x) = const x
adjustBy (Relative x) = (+ x)
-}
adjustBy = const . id

printToChat :: (Monad m, MonadReader r m, HasCommandEnv r, DiscordRest m) => String -> m ()
printToChat t = do
  cid <- view (cmdChannel.to channelId)
  void . doFetch $ CreateMessage cid (Text.pack t) Nothing

moveToVoiceChannel :: Channel -> CommandM ()
moveToVoiceChannel = const $ return () --TODO: implement voice support

nextSong :: CommandM ()
nextSong = return () --TODO: implement skip support in player

resolveQuery :: SongQuery -> Track
resolveQuery q = q
------------------------

{- TODO: permissions
permissive :: CommandInfo -> CommandM Bool
permissive cmd = do
   ps <- use $ activeUser.userPermissions
   return $ cmd^.cmdPermissions.to (containsAny ps)
     where
       containsAny ps = Set.null . Set.intersection ps
-}

{- TODO: delete if we can handle this entirely through optparse-applicative
helpCmd Nothing = listCommands
helpCmd (Just name) =
    case lookupCmd name of
      Just cmd -> printToChat $ prettyPrintCmd cmd
      Nothing -> printToChat $ "Command not found: " <> name

lookupCmd :: String -> Maybe CommandInfo
lookupCmd name = List.find (\c -> c^.cmdName == name) allCommands

allCommands :: [CommandInfo]
allCommands = [] -- TODO

listCommands :: CommandM ()
listCommands = printToChat =<<
    (joinOutput . fmap prettyPrintCmd <$> filterM permissive allCommands)

prettyPrintCmd cmd = _cmdName cmd <> ": " <> _cmdDesc cmd
joinOutput os = List.intercalate "\n" ("Commands:":os)
-}

-------

runCommand :: Snowflake -> CommandM a -> IO a
--TODO: replace defaults with values read from global state
runCommand uid cmd = do
    env <- makeEnv
    flip runReaderT env $ unwrapCmdM cmd

  where
    makeEnv = do
      np <- readTVarIO nowPlayingRef
      return $ CommandEnv { _nowPlaying = np
                          , _cmdUser = uid
                          }

instance DiscordAuth IO where
  auth    = return $ Bot "TOKEN"
  version = return "0.2.2"
  runIO   = id

data InitBot
instance EventMap InitBot (DiscordApp IO) where
  type Domain   InitBot = Init
  type Codomain InitBot = ()

  mapEvent _ _ = liftIO $ putStrLn "Hello, world!"
  --TODO

data UpdateGuildState
instance EventMap UpdateGuildState (DiscordApp IO) where
  type Domain UpdateGuildState = Guild
  type Codomain UpdateGuildState = ()

  mapEvent _ guild = undefined
  --TODO: store new info in DiscordState by patching it in

data UpdateVoiceState
instance EventMap UpdateVoiceState (DiscordApp IO) where
  type Domain UpdateVoiceState = Object
  type Codomain UpdateVoiceState = ()

  mapEvent _ obj = undefined
  -- TODO: patch updated voice state, maybe establish connection if all info is there

data UpdateVoiceServer
instance EventMap UpdateVoiceServer (DiscordApp IO) where
  type Domain UpdateVoiceServer = Object
  type Codomain UpdateVoiceServer = ()

  mapEvent _ obj = undefined
  -- TODO: patch updated voice state, maybe establish connection if all info is there

data RunCmd
instance EventMap RunCmd (DiscordApp IO) where
    type Domain RunCmd = Message
    type Codomain RunCmd = ()

    mapEvent _ (msg@Message{..}) =
        case Text.unpack messageContent of
            '!' : xs -> case OP.execParserPure OP.defaultPrefs
                                               (OP.info cmdParser mempty)
                                               (words xs) of
                OP.Success a -> liftIO $ runCommand (userId messageAuthor) a
                _ -> return ()
            _ -> return ()

type App
  =    (ReadyEvent :> InitBot)
  :<>: (GuildCreateEvent :> UpdateGuildState)
  :<>: (VoiceStateUpdateEvent :> UpdateVoiceState)
  :<>: (VoiceServerUpdateEvent :> UpdateVoiceServer)
  :<>: ((MessageCreateEvent :<>: MessageUpdateEvent) :> RunCmd)

instance EventHandler App IO

main :: IO ()
main = do
  initBotState
  runBot (Proxy :: Proxy (IO App))
    where
      initBotState = do
        st <- Acid.openLocalState emptyDB
        atomically $ putTMVar botStateRef st

emptyDB :: BotDB
emptyDB = BotDB { _userQueues=Map.empty, _playerVolume=30, _blacklist=Set.empty }
