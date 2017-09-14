{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest where

import Control.Concurrent (threadDelay)
import Control.Lens (view)
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Char8 (pack, ByteString)
import Data.Hashable
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import           Data.Text ( Text )
import qualified Data.Text as T (pack)
import Data.Time.Clock.POSIX
import Network.Discord.Types
import           Network.HTTP.Req ( (/:) )
import qualified Network.HTTP.Req as R
import System.Log.Logger

class RestMethod (a :: * -> *) b | a b -> b where
    execute :: (R.MonadHttp m, MonadRest m, MonadReader r m, HasAuthInfo r)
            => a b
            -> m b

class Monad m => MonadRest m where
  setRateLimit :: RestMethod a b => a b -> Int -> m ()
  getRateLimit :: RestMethod a b => a b -> m (Maybe Int)

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

-- | Construct base options with auth from Discord state
baseRequestOptions :: (MonadRest m, HasAuthInfo r, MonadReader r m) => m Option
baseRequestOptions = do
  a <- view auth
  v <- view version
  return $ R.header "Authorization" (pack . show $ a)
        <> R.header "User-Agent" (pack $ "DiscordBot (https://github.com/jano017/Discord.hs,"
                                      ++ v ++ ")")
infixl 5 //
(//) :: Show a => R.Url scheme -> a -> R.Url scheme
url // part = url R./: T.pack (show part)

type Option = R.Option 'R.Https

-- | Represtents a HTTP request made to an API that supplies a Json response
data JsonRequest r where
  Delete ::  FromJSON r                => R.Url 'R.Https      -> Option -> JsonRequest r
  Get    ::  FromJSON r                => R.Url 'R.Https      -> Option -> JsonRequest r
  Patch  :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r
  Post   :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r
  Put    :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r

fetch :: (MonadReader r m, HasAuthInfo r, FromJSON resp, MonadRest m, R.MonadHttp m)
      => JsonRequest resp
      -> m (R.JsonResponse resp)
fetch (Delete url      opts) = R.req R.DELETE url R.NoReqBody R.jsonResponse =<< (<> opts) <$> baseRequestOptions
fetch (Get    url      opts) = R.req R.GET    url R.NoReqBody R.jsonResponse =<< (<> opts) <$> baseRequestOptions
fetch (Patch  url body opts) = R.req R.PATCH  url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions
fetch (Post   url body opts) = R.req R.POST   url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions
fetch (Put    url body opts) = R.req R.PUT    url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions

makeRequest :: (R.MonadHttp m, MonadReader env m, HasAuthInfo env, FromJSON r, MonadRest m, RestMethod f r)
            => f r
            -> JsonRequest r
            -> m r
makeRequest req action = do
  waitForRateLimit req
  resp <- fetch action
  when (parseHeader resp "X-RateLimit-Remaining" 1 < 1) $
    setRateLimit req $ parseHeader resp "X-RateLimit-Reset" 0
  return $ R.responseBody resp
  where
    parseHeader :: R.HttpResponse resp => resp -> ByteString -> Int -> Int
    parseHeader resp header def = fromMaybe def $ decodeStrict =<< R.responseHeader resp header

instance Hashable (JsonRequest r) where
  hashWithSalt s (Delete url _)   = hashWithSalt s $ show url
  hashWithSalt s (Get    url _)   = hashWithSalt s $ show url
  hashWithSalt s (Patch  url _ _) = hashWithSalt s $ show url
  hashWithSalt s (Post   url _ _) = hashWithSalt s $ show url
  hashWithSalt s (Put    url _ _) = hashWithSalt s $ show url

-- | Base implementation of DoFetch, allows arbitrary HTTP requests to be performed
instance (FromJSON r) => RestMethod JsonRequest r where
  execute req = R.responseBody <$> fetch req

waitForRateLimit :: (MonadIO m, MonadRest m, RestMethod a b)
                 => a b -> m ()
waitForRateLimit endpoint = do
  rl <- getRateLimit endpoint
  case rl of
    Nothing -> return ()
    Just l -> do
      now <- liftIO (round <$> getPOSIXTime :: IO Int)
      when (l > now) . liftIO $ do
        infoM "Discord-hs.Rest" "Hit rate limit, backing off"
        threadDelay $ 1000000 * (l - now)
        infoM "Discord-hs.Rest" "Done waiting"
      return ()


-- | Data constructor for User requests. See
--   <https://discordapp.com/developers/docs/resources/user User API>
data UserRequest a where
  -- | Returns the 'User' object of the requester's account. For OAuth2, this requires
  --   the identify scope, which will return the object without an email, and optionally
  --   the email scope, which returns the object with an email.
  GetCurrentUser       :: UserRequest User
  -- | Returns a 'User' for a given user ID
  GetUser              :: Snowflake -> UserRequest User
  -- | Modify the requestors user account settings. Returns a 'User' object on success.
--ModifyCurrentUser    :: ToJSON a => a -> UserRequest User
  -- | Returns a list of user 'Guild' objects the current user is a member of.
  --   Requires the guilds OAuth2 scope.
--GetCurrentUserGuilds :: Range -> UserRequest Guild
  -- | Leave a guild.
--LeaveGuild           :: Snowflake -> UserRequest ()
  -- | Returns a list of DM 'Channel' objects
  GetUserDMs           :: UserRequest [Channel]
  -- | Create a new DM channel with a user. Returns a DM 'Channel' object.
  CreateDM             :: Snowflake -> UserRequest Channel

instance Hashable (UserRequest a) where
  hashWithSalt s GetCurrentUser           = hashWithSalt s  ("me"::Text)
  hashWithSalt s (GetUser _)              = hashWithSalt s  ("user"::Text)
  hashWithSalt s GetUserDMs               = hashWithSalt s  ("get_dms"::Text)
  hashWithSalt s (CreateDM _)             = hashWithSalt s  ("make_dm"::Text)

instance (FromJSON a) => RestMethod UserRequest a where
    execute = go
      where
        url = baseUrl /: "users"
        go :: (R.MonadHttp m, MonadRest m, MonadReader r m, HasAuthInfo r)
           => UserRequest a
           -> m a
        go r@GetCurrentUser = makeRequest r
            $ Get (url /: "@me") mempty

        go r@(GetUser user) = makeRequest r
            $ Get (url // user) mempty

        go r@GetUserDMs = makeRequest r
            $ Get (url /: "@me" /: "channels") mempty

        go r@(CreateDM user) = makeRequest r
            $ Post (url /: "@me" /: "channels")
                   (R.ReqBodyJson $ object [ "recipient_id" .= user ])
                   mempty

-- | Data constructor for Channel requests. See <https://discordapp.com/developers/docs/resources/Channel Channel API>
data ChannelRequest a where
  -- | Gets a channel by its id.
  GetChannel              :: Snowflake -> ChannelRequest Channel
  -- | Edits channels options.
--ModifyChannel           :: ToJSON a  => Snowflake -> a -> ChannelRequest Channel
  -- | Deletes a channel if its id doesn't equal to the id of guild.
--DeleteChannel           :: Snowflake -> ChannelRequest Channel
  -- | Gets a messages from a channel with limit of 100 per request.
--GetChannelMessages      :: Snowflake -> Range -> ChannelRequest [Message]
  -- | Gets a message in a channel by its id.
--GetChannelMessage       :: Snowflake -> Snowflake -> ChannelRequest Message
  -- | Sends a message to a channel.
  CreateMessage           :: Snowflake -> Text -> Maybe Embed -> ChannelRequest Message
  -- | Sends a message with a file to a channel.
--UploadFile              :: Snowflake -> FilePath -> ByteString -> ChannelRequest Message
  -- | Edits a message content.
--EditMessage             :: Message   -> Text -> Maybe Embed -> ChannelRequest Message
  -- | Deletes a message.
  DeleteMessage           :: Message   -> ChannelRequest ()
  -- | Deletes a group of messages.
--BulkDeleteMessage       :: Snowflake -> [Message] -> ChannelRequest ()
  -- | Edits a permission overrides for a channel.
--EditChannelPermissions  :: ToJSON a  => Snowflake -> Snowflake -> a -> ChannelRequest ()
  -- | Gets all instant invites to a channel.
--GetChannelInvites       :: Snowflake -> ChannelRequest Object
  -- | Creates an instant invite to a channel.
--CreateChannelInvite     :: ToJSON a  => Snowflake -> a -> ChannelRequest Object
  -- | Deletes a permission override from a channel.
--DeleteChannelPermission :: Snowflake -> Snowflake -> ChannelRequest ()
  -- | Sends a typing indicator a channel which lasts 10 seconds.
  TriggerTypingIndicator  :: Snowflake -> ChannelRequest ()
  -- | Gets all pinned messages of a channel.
--GetPinnedMessages       :: Snowflake -> ChannelRequest [Message]
  -- | Pins a message.
--AddPinnedMessage        :: Snowflake -> Snowflake -> ChannelRequest ()
  -- | Unpins a message.
--DeletePinnedMessage     :: Snowflake -> Snowflake -> ChannelRequest ()

instance Hashable (ChannelRequest a) where
  hashWithSalt s (GetChannel chan) = hashWithSalt s ("get_chan"::Text, chan)
  hashWithSalt s (CreateMessage chan _ _) = hashWithSalt s ("msg"::Text, chan)
  hashWithSalt s (DeleteMessage (Message _ chan _ _ _ _ _ _ _ _ _ _ _ _)) =
    hashWithSalt s ("get_msg"::Text, chan)
  hashWithSalt s (TriggerTypingIndicator chan)  = hashWithSalt s ("tti"::Text, chan)

instance (FromJSON a) => RestMethod ChannelRequest a where
    execute = go
      where
        maybeEmbed :: Maybe Embed -> [(Text, Value)]
        maybeEmbed = maybe [] $ \embed -> [ "embed" .= embed ]

        url = baseUrl /: "channels"
        go :: (R.MonadHttp m, MonadRest m, MonadReader r m, HasAuthInfo r)
           => ChannelRequest a
           -> m a
        go r@(GetChannel chan) =
            makeRequest r
                $ Get (url // chan) mempty
        go r@(CreateMessage chan msg embed) =
            makeRequest r
                $ Post (url // chan /: "messages")
                       (R.ReqBodyJson . object $
                            [ "content" .= msg ] <> maybeEmbed embed)
                       mempty
        go r@(DeleteMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _)) =
            makeRequest r
                $ Delete (url // chan /: "messages" // msg) mempty
        go r@(TriggerTypingIndicator chan) =
            makeRequest r
                $ Post (url // chan /: "typing") R.NoReqBody mempty

-- | Data constructor for Guild requests. See
--   <https://discordapp.com/developers/docs/resources/guild Guild API>
data GuildRequest a where
  -- | Returns the new 'Guild' object for the given id
  GetGuild                 :: Snowflake -> GuildRequest Guild
  -- | Modify a guild's settings. Returns the updated 'Guild' object on success. Fires a
  --   Guild Update 'Event'.
--ModifyGuild              :: ToJSON a => Snowflake -> a -> GuildRequest Guild
  -- | Delete a guild permanently. User must be owner. Fires a Guild Delete 'Event'.
--DeleteGuild              :: Snowflake -> GuildRequest Guild
  -- | Returns a list of guild 'Channel' objects
  GetGuildChannels         :: Snowflake -> GuildRequest [Channel]
  -- | Create a new 'Channel' object for the guild. Requires 'MANAGE_CHANNELS'
  --   permission. Returns the new 'Channel' object on success. Fires a Channel Create
  --   'Event'
--CreateGuildChannel       :: ToJSON a => Snowflake -> a -> GuildRequest Channel
  -- | Modify the positions of a set of channel objects for the guild. Requires
  --   'MANAGE_CHANNELS' permission. Returns a list of all of the guild's 'Channel'
  --   objects on success. Fires multiple Channel Update 'Event's.
--ModifyChanPosition       :: ToJSON a => Snowflake -> a -> GuildRequest [Channel]
  -- | Returns a guild 'Member' object for the specified user
  GetGuildMember           :: Snowflake -> Snowflake -> GuildRequest Member
  -- | Returns a list of guild 'Member' objects that are members of the guild.
--ListGuildMembers         :: Snowflake -> Range -> GuildRequest [Member]
  -- | Adds a user to the guild, provided you have a valid oauth2 access token
  --   for the user with the guilds.join scope. Returns the guild 'Member' as the body.
  --   Fires a Guild Member Add 'Event'. Requires the bot to have the
  --   CREATE_INSTANT_INVITE permission.
--AddGuildMember           :: ToJSON a => Snowflake -> Snowflake -> a
--                              -> GuildRequest Member
  -- | Modify attributes of a guild 'Member'. Fires a Guild Member Update 'Event'.
--ModifyGuildMember        :: ToJSON a => Snowflake -> Snowflake -> a
--                              -> GuildRequest ()
  -- | Remove a member from a guild. Requires 'KICK_MEMBER' permission. Fires a
  --   Guild Member Remove 'Event'.
--RemoveGuildMember        :: Snowflake -> Snowflake -> GuildRequest ()
  -- | Returns a list of 'User' objects that are banned from this guild. Requires the
  --   'BAN_MEMBERS' permission
--GetGuildBans             :: Snowflake -> GuildRequest [User]
  -- | Create a guild ban, and optionally Delete previous messages sent by the banned
  --   user. Requires the 'BAN_MEMBERS' permission. Fires a Guild Ban Add 'Event'.
--CreateGuildBan           :: Snowflake -> Snowflake -> Integer -> GuildRequest ()
  -- | Remove the ban for a user. Requires the 'BAN_MEMBERS' permissions.
  --   Fires a Guild Ban Remove 'Event'.
--RemoveGuildBan           :: Snowflake -> Snowflake -> GuildRequest ()
  -- | Returns a list of 'Role' objects for the guild. Requires the 'MANAGE_ROLES'
  --   permission
--GetGuildRoles            :: Snowflake -> GuildRequest [Role]
  -- | Create a new 'Role' for the guild. Requires the 'MANAGE_ROLES' permission.
  --   Returns the new role object on success. Fires a Guild Role Create 'Event'.
--CreateGuildRole          :: Snowflake -> GuildRequest Role
  -- | Modify the positions of a set of role objects for the guild. Requires the
  --   'MANAGE_ROLES' permission. Returns a list of all of the guild's 'Role' objects
  --   on success. Fires multiple Guild Role Update 'Event's.
--ModifyGuildRolePositions :: ToJSON a => Snowflake -> [a] -> GuildRequest [Role]
  -- | Modify a guild role. Requires the 'MANAGE_ROLES' permission. Returns the
  --   updated 'Role' on success. Fires a Guild Role Update 'Event's.
--ModifyGuildRole          :: ToJSON a => Snowflake -> Snowflake -> a
--                              -> GuildRequest Role
  -- | Delete a guild role. Requires the 'MANAGE_ROLES' permission. Fires a Guild Role
  --   Delete 'Event'.
--DeleteGuildRole          :: Snowflake -> Snowflake -> GuildRequest Role
  -- | Returns an object with one 'pruned' key indicating the number of members
  --   that would be removed in a prune operation. Requires the 'KICK_MEMBERS'
  --   permission.
--GetGuildPruneCount       :: Snowflake -> Integer -> GuildRequest Object
  -- | Begin a prune operation. Requires the 'KICK_MEMBERS' permission. Returns an
  --   object with one 'pruned' key indicating the number of members that were removed
  --   in the prune operation. Fires multiple Guild Member Remove 'Events'.
--BeginGuildPrune          :: Snowflake -> Integer -> GuildRequest Object
  -- | Returns a list of 'VoiceRegion' objects for the guild. Unlike the similar /voice
  --   route, this returns VIP servers when the guild is VIP-enabled.
  GetGuildVoiceRegions     :: Snowflake -> GuildRequest [VoiceRegion]
  -- | Returns a list of 'Invite' objects for the guild. Requires the 'MANAGE_GUILD'
  --   permission.
--GetGuildInvites          :: Snowflake -> GuildRequest [Invite]
  -- | Return a list of 'Integration' objects for the guild. Requires the 'MANAGE_GUILD'
  --   permission.
--GetGuildIntegrations     :: Snowflake -> GuildRequest [Integration]
  -- | Attach an 'Integration' object from the current user to the guild. Requires the
  --   'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
--CreateGuildIntegration   :: ToJSON a => Snowflake -> a -> GuildRequest ()
  -- | Modify the behavior and settings of a 'Integration' object for the guild.
  --   Requires the 'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
--ModifyGuildIntegration   :: ToJSON a => Snowflake -> Snowflake -> a -> GuildRequest ()
  -- | Delete the attached 'Integration' object for the guild. Requires the
  --   'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
--DeleteGuildIntegration   :: Snowflake -> Snowflake -> GuildRequest ()
  -- | Sync an 'Integration'. Requires the 'MANAGE_GUILD' permission.
--SyncGuildIntegration     :: Snowflake -> Snowflake -> GuildRequest ()
  -- | Returns the 'GuildEmbed' object. Requires the 'MANAGE_GUILD' permission.
--GetGuildEmbed            :: Snowflake -> GuildRequest GuildEmbed
  -- | Modify a 'GuildEmbed' object for the guild. All attributes may be passed in with
  --   JSON and modified. Requires the 'MANAGE_GUILD' permission. Returns the updated
  --   'GuildEmbed' object.
--ModifyGuildEmbed         :: Snowflake -> GuildEmbed -> GuildRequest GuildEmbed

instance Hashable (GuildRequest a) where
  hashWithSalt s (GetGuild g)              = hashWithSalt s ("guild"::Text, g)
  hashWithSalt s (GetGuildChannels g)      = hashWithSalt s ("guild_chan"::Text, g)
  hashWithSalt s (GetGuildMember g _)      = hashWithSalt s ("guild_memb"::Text, g)
  hashWithSalt s (GetGuildVoiceRegions g)  = hashWithSalt s ("guild_voice"::Text, g)

instance (FromJSON a) => RestMethod GuildRequest a where
  execute = go
    where
      url = baseUrl /: "guilds"
      go :: (R.MonadHttp m, MonadRest m, MonadReader r m, HasAuthInfo r)
           => GuildRequest a -> m a
      go r@(GetGuild guild) = makeRequest r
        $ Get (url // guild) mempty
      go r@(GetGuildChannels guild) = makeRequest r
        $ Get (url // guild /: "channels") mempty
      go r@(GetGuildMember guild member) = makeRequest r
        $ Get (url // guild /: "members" // member) mempty
      go r@(GetGuildVoiceRegions guild) = makeRequest r
        $ Get (url // guild /: "regions") mempty
