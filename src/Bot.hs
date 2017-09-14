{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows, TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, LambdaCase #-}

module Bot where

import           Conduit
import           Control.Applicative
import           Control.Auto
import           Control.Auto.Blip
import           Control.Auto.Collection
import           Control.Auto.Effects
import           Control.Auto.Interval
import           Control.Auto.Switch
import           Control.Concurrent hiding ( yield )
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Lens hiding ( (:<), (|>) )
import           Control.Monad ( (<=<), unless )
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans ()
import           Data.Aeson ( Value(Object), eitherDecode, encode, decode )
import           Data.Aeson.Lens
import           Data.ByteString
import           Data.Conduit.Combinators ( omapME )
import           Data.Conduit.Network.UDP ( sinkSocket )
import           Data.Conduit.Process ( CreateProcess, sourceProcessWithStreams )
import           Data.Foldable ( toList )
import qualified Data.HashMap.Lazy as HashMap
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Monoid as Monoid
import           Data.Sequence ( (|>), Seq, ViewL(..) )
import qualified Data.Sequence as Seq
import           Data.Serialize ( Serialize )
import qualified Data.Serialize as Serialize
import           Data.Text ( Text )
import qualified Data.Text as Text
import           Data.Serialize.Text ()
import           Data.Word ( Word8 )
import           GHC.Generics ( Generic )
import           Network.Discord.Types
--import           Network.HTTP.Req ( (/:) )
import qualified Network.HTTP.Req as R
import           Network.Socket ( Socket )
import qualified Network.WebSockets as WS
import           Prelude hiding ( (.), id )
import qualified Wuss as WS

import Rest

------------------

type UserId = Snowflake
type ChannelId = Snowflake
type Track = Text

data UserInfo = UserInfo { _nickname :: Maybe Text
                         , _username :: Text
                         }
makeClassy ''UserInfo

data Command = Queue (Maybe Track)
             | NowPlaying
             | Skip
             | Summon
             | Volume Int
             | Join
             | Leave
makeClassyPrisms ''Command

data CommandInfo = CommandInfo { _cmdUser :: User
                               , _cmdChannel :: ChannelId
                               , _cmd :: Command
                               }
makeClassy ''CommandInfo

data TrackInfo = TrackInfo { _trackName :: Track
                           , _trackUser :: UserId
                           , _trackUsername :: Text --TODO: remove once name tracking is added
                           , _trackUrl :: Text
                           }
  deriving (Generic)
makeClassy ''TrackInfo
instance Serialize TrackInfo

data PlayerInfo = PlayerInfo { _volume :: Int
                             , _nowPlaying :: TrackInfo
                             }
makeClassy '' PlayerInfo

type Queue = Seq TrackInfo

data BotEvent = GatewayEvent Payload
              | TrackEnded
makeClassyPrisms ''BotEvent

data PlayerEvent = Next | Stop | Pause | Play
makeClassyPrisms ''PlayerEvent

data PlayerState = Playing TrackInfo | Paused TrackInfo | Stopped

-- orphan
instance Serialize Object where
  get = do
    bs <- Serialize.get
    case decode bs of
      Just (Object o) -> return o
      _ -> fail "Could not parse JSON Object"
  put = Serialize.put . encode . Object

------------------

makePrisms ''Payload
makePrisms ''Event

-- sends heartbeat indefinitely
heartbeat :: MonadIO m => Int -> m ()
heartbeat interval = do
    send
    sleep
    unlessM check $
        heartbeat interval
  where
    -- TODO: implement
    send, sleep :: MonadIO m => m ()
    send = undefined
    sleep = undefined

    check :: MonadIO m => m Bool
    check = undefined

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb m = do
  b <- mb
  Control.Monad.unless b m

resetTimeout :: MonadIO m => m ()
resetTimeout = undefined

-- connection info
data ConnectionState = ConnectionState { _sessionId :: Maybe Int
                                       , _sequenceId :: Maybe Int
                                       }

type DiscordState m a = Auto m Event a

class Monad m => MonadGate m where
  identify :: m ()
  moveBot :: ChannelId -> m ()

class Monad m => MonadYTDL m where

class Monad m => MonadPlayer m where
  setVolume :: Int -> m ()

getGateway :: R.MonadHttp m => m (Maybe Text)
getGateway = (parseJsonResponse . R.responseBody) <$> getReq gatewayUrl
  where
    parseJsonResponse :: Value -> Maybe Text
    parseJsonResponse = preview $ key "url" . _String
    gatewayUrl = R.https "discordapp.com" R./: "api" R./: "gateway"
    getReq url = R.req R.GET url R.NoReqBody R.jsonResponse mempty

data PlayerData = PlayerData { _playStateTVar :: TVar PlayerState
                             , _volumeIORef :: IORef Word8
                             , _voiceSocketRef :: TMVar Socket
                             }
makeClassy ''PlayerData


ytdl :: TrackInfo -> CreateProcess
ytdl t = undefined --TODO

-- player thread
play :: forall r m. (MonadIO m, MonadReader r m, HasPlayerData r) => TChan BotEvent -> m ()
play outChan = do
    ps <- view playStateTVar
    t <- liftIO . atomically $ do
             cmd <- readTVar ps
             case cmd of
                 Playing t -> return t
                 _ -> retry
    playTrack t
    play outChan
  where
    playTrack :: TrackInfo
              -> m ()
    playTrack t = liftIO . void $
        sourceProcessWithStreams (ytdl t)
                                 (return ()) -- stdin
                                 (playerControlC -- TODO: this is expeted to be IO only, can't use `m`
                                      =$= applyVolumeC
                                      =$= terminateWithSilenceC
                                      =$= attachSocketC
                                      =$= sinkVoiceSocket)
                                 (return ()) --TODO: stderr

    -- TODO: handle skips
    playerControlC :: Conduit ByteString m ByteString
    playerControlC = do
        playing <- lift $ do
                       ps <- view playStateTVar
                       liftIO . atomically $
                           readTVar ps >>=
                               \case
                                   Playing _ -> return True
                                   Paused _ -> retry
                                   Stopped -> return False
        if playing
            then await >>=
                \case
                    Just x -> yield x >> playerControlC
                    Nothing -> return ()
            else return ()

    terminateWithSilenceC = do
        await >>=
            \case
                Nothing -> yield silence
                (Just x) -> yield x >> terminateWithSilenceC

    silence = pack [ 0xF8, 0xFF, 0xFE ]

    applyVolumeC :: Conduit ByteString m ByteString
    applyVolumeC = do
        vRef <- lift $ view volumeIORef
        omapME $ \b -> (b *) <$> (liftIO $ readIORef vRef)

    attachSocketC :: Conduit ByteString m (ByteString, Socket)
    attachSocketC = mapMC $
        \a -> do
            sockRef <- view voiceSocketRef
            s <- liftIO . atomically $ readTMVar sockRef
            return (a, s)

    sinkVoiceSocket :: Sink (ByteString, Socket) m ()
    sinkVoiceSocket = awaitForever $ \(a, s) -> yield a =$= sinkSocket s


gateway :: forall m r.
        (MonadRest m, MonadGate m, R.MonadHttp m, MonadYTDL m, MonadFix m, MonadPlayer m,
         MonadReader r m, HasAuthInfo r)
        => Auto m BotEvent ()
gateway = proc evt -> do
  p <- emitJusts (^? _GatewayEvent) -< evt

  -- wait for Hello payload
  helloBlip <- mapMaybeB (^? _Hello) -< p
  switchOnF start (pure ()) -< (evt, helloBlip)
  id -< ()
    where
      start hb = proc evt -> do

        -- init heartbeat and send identity to gateway
        -- TODO: could maybe move these up a level?
        execOnce (heartbeat hb) -< () -- TODO: need a way to kill heartbeat
                                      --       thread. Effect should return a
                                      --       handle of some sort. Can use
                                      --       "cache" instead of execOnce.
        execOnce identify -< ()

        -- gateway events
        p <- emitJusts (^? _GatewayEvent) -< evt

        -- update heartbeat timeout when HeartbeatAcks are received
        effectB resetTimeout . mapMaybeB (^? _HeartbeatAck) -< p

        -- wait for Ready event
        readyBlip <- mapMaybeB ((^? _Right . _Ready) . parseDispatch) -< p
        --TODO: should we use switchFromF_?
        switchOnF_ ready (pure ()) -< (evt, readyBlip)

        id -< ()

      ready (Init v user privChans guilds sessionId) = proc evt -> do
        -- TODO: report sessionId, only once

        p <- emitJusts (^? _GatewayEvent) -< evt
        eventB <- mapMaybeB (preview _Right . parseDispatch) -< p
        -- TODO: report sequenceId, once for each dispatch

        -- Map UserId Object[User]
        voiceChanMap
          <- fromInterval Map.empty . usersI . joinB . perBlip userChanEvent
          -< eventB

        -- TODO: need to track nicks and usernames, requires tracking guild
        --       creation to get all users, as well as member and user updates.
        --       For larger guilds, can't get all members at once. All-in-all,
        --       it's kind of a bitch.
        --
        --       One option is to only track users who have queues. We could get
        --       the initial username via the MessageCreated event (comes w/ a
        --       User object) and then track updates until she leaves the
        --       rotation (either via message, by leaving the voice channel, or
        --       removal from guild). Unfortunately, this doesn't give us
        --       nicknames, only usernames.
        --
        --       For now, we just store the username used at the time a track is
        --       added to the queue.
        -----------------
        -- Map UserId UserInfo
        {-
        userMap
          <- userMapI . mapMaybeB userUpdateEvent
          -< eventB
        -}

        commandB
          <- mapMaybeB (preview _MessageCreate >=> parseMessage)
          -< eventB

        arrMB notifyTyping . modifyBlips _cmdChannel -< commandB

        -- !join
        joinCmdB
          <- modifyBlips (joinRotation . userId . _cmdUser)
             . filterB (has (cmd._Join))
          -< commandB
        -- !leave
        leaveCmdB
          <- modifyBlips (leaveRotation . userId . _cmdUser)
             . filterB (has (cmd._Leave))
          -< commandB

        nextTrackB <- emitJusts (^? _TrackEnded) -< evt

        rec -- dj play order
            -- Seq UserId
            rotation <- scanB (&) Seq.empty
              -< mergeLs [ joinCmdB
                         , leaveCmdB
                         , rotate db <$ nextTrackB
                         ]
            nextDjMB <- perBlip nextDj -< rotation <$ nextTrackB
            nextDjB <- mapMaybeB id -< nextDjMB
            noDjB <- mapMaybeB (^? _Nothing) -< nextDjMB

            -- !queue <track>
            tB
              <- mapMaybeMB resolveTrack . mapMaybeB (^? cmd._Queue._Just)
              -< commandB
            rTrackB <- combineB (curry $ second (userId . _cmdUser)) -< (tB, commandB)

            dbB <- perBlip (sealState dbAuto Map.empty)
              -< mergeLs [ ((Nothing <$) . modify . uncurry addTrack) <$> rTrackB
                         , (state . popTrack) <$> nextDjB
                         ]
            db <- fromInterval Map.empty . hold -< fst <$> dbB

        -- !queue
        printQueueB
          <- modifyBlips (_cmdChannel &&& (userId . _cmdUser))
             . filterB (has $ cmd._Queue._Nothing)
          -< commandB
        let
          queueChan :: Blip (Snowflake, Maybe Queue)
          queueChan = printQueueB & mapped._2 %~ flip Map.lookup db
        arrMB (uncurry sendQueue) . mapMaybeB sequenceA
          -< queueChan

        newTrackB <- joinB -< snd <$> dbB
        -- TODO: check interval API for a better way to do this
        currentTrack
          <- fromInterval Nothing . hold
          -< mergeLs [ Just <$> newTrackB
                     , Nothing <$ noDjB
                     ]

        -- !np
        printNpB
          <- modifyBlips _cmdChannel . filterB (has $ cmd._NowPlaying)
          -< commandB
        let npInfo = do
            TrackInfo{..} <- currentTrack
            -- u <- Map.lookup _trackUser userMap
            -- name <- (Object u) ^? key "username" . _String
            return (_trackName, _trackUsername)
        arrMB (uncurry sendNowPlaying) -< (,npInfo) <$> printNpB

        -- !volume <amt>
        arrMB setVolume . mapMaybeB (^? cmd._Volume) -< commandB

        -- !summon
        summonUserB
          <- modifyBlips (userId . _cmdUser) . filterB (has (cmd._Summon))
          -< commandB
        arrMB moveBot . mapMaybeB (uncurry Map.lookup)
          -< (,voiceChanMap) <$> summonUserB

        -- !skip
        skipCmdB <- modifyBlips (userId . _cmdUser) . filterB (has (cmd._Skip)) -< commandB
        npUidB <- onJusts -< _trackUser <$> currentTrack
        (instaSkipB, voteSkipB)
          <- splitB (uncurry (==)) . combineB (,)
          -< (skipCmdB, npUidB)

        -- TODO: figure out skip threshold and reset logic. switchOnF or resetOn perhaps?

        -- TODO: For instaskips, do we merge that with NextTrack events?
        --       Or do we just notify the player thread, which will then propagate the NextTrack event?

        id -< ()

      notifyTyping :: ChannelId -> m ()
      notifyTyping = void . execute . TriggerTypingIndicator

      sendQueue :: ChannelId -> Queue -> m ()
      sendQueue cid q = void . execute $ CreateMessage cid txt Nothing
        where
          txt = Data.Foldable.toList q
            & Prelude.zipWith (\i t -> i <> ". " <> _trackName t)
                              ((Text.pack . show) <$> [1..])
            & Text.unlines

      sendNowPlaying :: ChannelId -> Maybe (Text, Text) -> m ()
      sendNowPlaying _ Nothing = return () --TODO: help message?
      sendNowPlaying cid (Just (tName, uName)) = void . execute $ CreateMessage cid txt Nothing
        where
          txt = uName <> " is now playing: " <> tName

      dbAuto :: Auto (StateT a m) (StateT a m (Maybe b)) (a, Blip b)
      dbAuto = proc mb -> do
        aB <- onJusts . effects -< mb
        s <- effect get -< ()
        id -< (s, aB)

      nextDj = arr (Seq.viewl >>> \case u :< _ -> Just u; _ -> Nothing)

      combineB :: (a -> b -> c) -> Auto m (Blip a, Blip b) (Blip c)
      combineB f = proc (l, r) ->
        joinB -< fmap (\c -> fmap (f c) r) l

      mapMaybeMB :: (a -> m (Maybe b)) -> Auto m (Blip a) (Blip b)
      mapMaybeMB f = joinB . perBlip (onJusts . arrM f)

      popTrack :: UserId -> Map UserId Queue -> (Maybe TrackInfo, Map UserId Queue)
      popTrack uid = (at uid._Just) (pop >>> first Monoid.First) >>> first Monoid.getFirst
        where
          pop :: Queue -> (Maybe TrackInfo, Queue)
          pop q = case Seq.viewl q of
            t :< ts -> (Just t, ts)
            _ -> (Nothing, Seq.empty)

      addTrack :: TrackInfo -> UserId -> Map UserId Queue -> Map UserId Queue
      addTrack i = Map.alter (\case Nothing -> Just (Seq.singleton i); Just q -> Just (q |> i))

      joinRotation, leaveRotation :: UserId -> Seq UserId -> Seq UserId
      joinRotation u us = case Seq.elemIndexL u us of
        Just _ -> us
        Nothing -> us |> u
      leaveRotation u = Seq.filter (/= u)

      rotate :: Map UserId Queue -> Seq UserId -> Seq UserId
      rotate db = rot1 >>> rotateEmpty
      -- TODO: maybe we should just skip empties instead of moving them all the way to the back
        where
          rot1 = Seq.viewl >>> \case u :< us -> us |> u; _ -> Seq.empty
          rotateEmpty =
            uncurry (flip mappend)
            . Seq.spanl (maybe False Seq.null . flip Map.lookup db)

      parseMessage :: Message -> Maybe CommandInfo
      parseMessage Message{..} = undefined --TODO

      {-
      userUpdateEvent :: Event -> Maybe (UserId, Object)
      userUpdateEvent (UserUpdate o) = do
        uid <- (Object o) ^? (key "user_id" . _Integral)
        return (uid, o)
      userUpdateEvent _ = Nothing

      userMapI = scanB (flip . uncurry $ Map.insertWith patchUser) Map.empty

      patchUser :: Object -> Object -> Object
      patchUser = HashMap.union
      -}

      userChanEvent :: Auto m Event (Blip (UserId, Maybe ChannelId))
      userChanEvent = onJusts . arr (^? _VoiceStateUpdate . to readE . _Just)
        where
          --TODO: gross, gotta be a better way
          readE :: Object -> Maybe (UserId, Maybe ChannelId)
          readE o = do
            uid <- Object o ^? (key "user_id" . _Integral)
            return (uid, Object o ^? key "channel_id" . _Integral)

      usersI :: Interval m (Blip (UserId, Maybe ChannelId)) (Map UserId ChannelId)
      usersI = hold_ . perBlip (gather_ $ const userChanI)

      userChanI :: Interval m (Maybe ChannelId) ChannelId
      userChanI = before . (arr (fromMaybe undefined) &&& onJusts)

resolveTrack :: MonadYTDL m => Track -> m (Maybe TrackInfo)
resolveTrack = undefined -- TODO

(=>^) :: Monad m => (b -> m c) -> (c -> d) -> b -> m d
k =>^ fn = (return . fn) <=< k

connectGateway :: String -> IO ()
connectGateway hostname = do
    sendChan <- newTChanIO
    forkPlayer sendChan
    WS.runSecureClient hostUrl 443 "/" (app sendChan)
  where
    hostUrl :: String
    hostUrl = hostname ++ "?v=6&encoding=json" --TODO: v=5?

    app :: TChan BotEvent -> WS.ClientApp ()
    app sendChan connection = do

      let controllerThread controller = do
            x <- atomically $ readTChan sendChan
            ((), c') <- stepAuto controller x
            controllerThread c'

      forkIO $ controllerThread gateway

      let receive = do
            x <- WS.receiveData connection
            case eitherDecode x of
              Left err -> receive
              Right msg -> do
                atomically $ writeTChan sendChan $ GatewayEvent msg
                receive

      receive   -- TODO: we need a tchan to communicate with the controller
                --       auto. we loop waiting for data, then send it over the
                --       tchan.
                --
                --       Q: how do we send stuff over the websocket via the
                --       controller? separate tchan? separate threads for
                --       sending/receiving?

    forkPlayer sendChan =
      forkIO $ runReaderT (play sendChan)
                          (PlayerData undefined undefined undefined)

{-
      guildEvent :: Auto m Event (Blip (Snowflake, Event))
      guildEvent = onJusts . arr (\e -> (,e) <$> readE e)
        where
          readE :: Event -> Maybe Snowflake
          {-
          readE (ChannelCreate (Voice {..})) = Just channelGuild
          readE (ChannelUpdate (Voice {..})) = Just channelGuild
          readE (ChannelDelete (Voice {..})) = Just channelGuild
          readE (GuildCreate g) = Just $ guildId g
          readE (GuildUpdate g) = Just $ guildId g
          readE (GuildDelete g) = Just $ guildId g
          -}
          readE (VoiceStateUpdate o) = (Object o) ^? key "guild_id" . _Integral
          readE _ = Nothing

      guildsI :: Interval m (Blip (Snowflake, Event)) (Map Snowflake GuildInfo)
      guildsI = hold_ . perBlip (gather_ $ const guildI)

      guildI :: Interval m Event GuildInfo
      guildI = before . (guildUpdate &&& emitJusts (^? _GuildDelete))

      guildUpdate :: Auto m Event GuildInfo
      guildUpdate = undefined

      channelsI :: Interval m (Blip (Snowflake, Event)) (Map Snowflake ChannelInfo)
      channelsI = hold_ . perBlip (gather_ $ const channelI)

      channelI :: Interval m Event ChannelInfo
      channelI = before . (channelUpdate &&& emitJusts (^? _ChannelDelete))

      channelUpdate = undefined
-}
{-
      guildUpdate = sealState_ (effect (use guildInfo) . arrM update) defaultGuildInfo
        where
          update :: (MonadState s n, HasGuildInfo s) => Event -> n ()
          update _ = return ()
-}


{- GATEWAY CONNECTION

1. Get gateway URL via REST:
   GET https://discordapp.com/api/gateway
   Result can be cached: if cached URL can no longer be used to connect, discard and get a new one.
   Example response:
     {
       "url": "wss://gateway.discord.gg/"
     }

2. Connect to gateway via wuss. <step 1 url>?v=6&encoding=json
                                               ^
                                               |
                                               +- API Version

3. Receive OP10 Hello payload, containing heartbeat_interval

4. Start sending OP1 Heartbeat every heartbeat_interval (see step 3) millis.
   Heartbeats contain the sequence_id last received from the gateway.
     - Only OP0 Dispatch payloads contain a sequence_id.
     - If no sequence_id has been received yet, send NULL.
   If no OP11 HeartbeatAck payload has been received since last heartbeat, see step 8.

5. Send OP2 Identify.

6. Receive OP0 Dispatch containing READY event.
   Important things learned here:
     session_id: used for OP6 Resume
     guilds: contains "Unavailable Guild" objects
             Guilds become available asynchronously via GUILD_CREATE events
     private_channels: user direct-messages (DMs)

7. Start receiving Events. We're up and running!

....until....

8. Disconnection occurs, either by no OP11 HeartbeatAck or websocket close.
   We can resume:
     1. close websocket with a non 1000 close code
     2. reconnect
     3. attempt to resume (OP6 Resume instead of normal OP2 Identify)
        This requires session_id (from READY event) and sequence_id (from whatever last received OP0 Dispatch payload was).
     4. If resume succeeds, everything will continue to function as normal. If not, see step 9.

9. Can't resume (OP9 InvalidSession)
   Discard all tracked state, wait 1-5 seconds (docs say random amount of time in that interval), then send OP2 Identify.

-}

{- VOICE CONNECTION

1. Connect to Gateway (GATEWAY CONNECTION steps 1-7)

2. Send OP4 GatewayVoiceStateUpdate

3. Wait for VOICE_STATE_UPDATE event and VOICE_SERVER_UPDATE event.
   The former contains "session_id", the latter "token" and "endpoint"
   TODO: VOICE_STATE_UPDATE is fired for *all* user voice state changes. Do we need to ensure the user id matches ours?

4. Connect to voice server gateway, specified by "endpoint" in step 3, via wuss.

5. Send VOP0 Identify, using the info from step 3. "server_id" refers to the guild id

6. Receive VOP2 Ready payload. Contains "port" and "heartbeat_interval"

7. Start sending heartbeats at the interval specified by "heartbeat_interval" in the payload received in step 6 (in millis).

8. Open UDP connection to "endpoint" (from step 3), with port given by "port" (from step 6).

9. Send VOP1 SelectProtocol.
   TODO: If we *need* to specify our own port for receiving UDP voice data, follow IP Discovery guide:
         https://discordapp.com/developers/docs/topics/voice-connections#ip-discovery

10. Receive VOP4 SessionDescription (payload contents undocumented, potentially empty?)

11. Start sending voice data.
    Audio encoded w/ Opus, stereo, 48Khz
    See https://discordapp.com/developers/docs/topics/voice-connections#encrypting-and-sending-voice
-}
