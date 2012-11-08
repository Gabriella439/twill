{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

-- | General API types for communicating with Twilio. The names of the
-- types are fairly generic and thus are expected to be imported
-- qualified.
module Service.Twilio.Types where

import Prelude hiding (id)

import Text.Printf
import Data.Data
import Data.Char
import Data.String
import Data.Monoid
import Data.Default
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as A
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.DateTime () -- imports 'Arbitrary UTCTime'
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Applicative
import Control.Error
import Test.QuickCheck

-- | A price in cents USD.
type Price = Int

-- | A Twilio identity, either for an application or a user,
-- corresponds to a Twilio SID and Secret Token, in order.
data Passport = Passport ByteString ByteString
              deriving (Eq, Data, Typeable)

-- | Specialized to elide secret token information
instance Show Passport where
  showsPrec _ (Passport sid _) rest =
    "Passport " ++ show sid ++ "[elided]" ++ rest

-- | Phone numbers are just strings containing only numbers with some
-- specialized creation logic. In particular, a phone number can be
-- any string of numbers and are somewhat assumed to be US numbers
-- (country code "+1"). The input data is preserved---a number entered
-- without a country code is stored, printed, and returned without a
-- country code---but 'Eq' is overrided with some assumptions.
newtype PhoneNumber = PhoneNumber { getPhoneNumber :: ByteString }
                    deriving (Show, Data, Typeable)

-- | TODO Make this do more than lift the equality judgement
instance Eq PhoneNumber where
  (PhoneNumber a) == (PhoneNumber b) = a == b

-- | Converts a string to a more canonical PhoneNumber type
--
-- > fromString = PhoneNumber . BC.pack . filter isDigit
--
instance IsString PhoneNumber where
  fromString = PhoneNumber . BC.pack . filter isDigit

-- | The status of an outbound SMS in Twilio's system. The state
-- machine for messages is @(Queued -> Sending -> {Sent, Failed})@.
data SendStatus =
  -- | 'Queued' messages have been received by Twilio but not yet sent.
  Queued |
  -- | 'Sending' messages are in the process of sending.
  Sending |
  -- | 'Sent' messages have been confirmed sent and thus have both the
  -- send date and the price charged.
  Sent UTCTime Price |
  -- | 'Failed' messages have been confirmed failed to send by the
  -- carrier and usually suggest an unregistered phone number.
  Failed 
  deriving (Show, Eq, Ord, Data, Typeable)

-- | Assumes the default is a just sent message, thus it's queued
instance Default SendStatus where
  def = Queued

-- | The type of API used to send an outgoing message
data APIKind = API | Reply deriving (Show, Eq, Data, Typeable)

-- | Assumes the default is API usage
instance Default APIKind where
  def = API

-- | The type of SMS, be it incoming or outgoing
data SMSKind = Outbound APIKind SendStatus | Inbound
             deriving (Show, Eq, Data, Typeable)

-- | Assumes we're sending a default outbound message
instance Default SMSKind where
  def = Outbound def def

-- | A fixed enum representation of the API version since
-- 'ByteString's are far too permissive.
data APIVersion = Api20100401 deriving (Eq, Ord, Data, Typeable)

instance Show APIVersion where
  show Api20100401 = "2010-04-01"

-- | Defaults to the most recent API, "2010-04-01"
instance Default APIVersion where
  def = Api20100401

-- | A unique identifier for a message based on the account SID, API
-- version, and message SID. This can be used to construct the
-- canonical Twilio API URI.
data Id = Id { account :: ByteString, version :: APIVersion, sid :: ByteString }
        deriving (Show, Eq, Data, Typeable)

-- | Create a URI derived from an 'Id'
uri :: ByteString -> Id -> ByteString
uri typ (Id { .. }) = "/" <> fromString (show version)
                      <> "/Accounts/" <> account
                      <> "/" <> typ <> "/"
                      <> sid

data SMSCore =
  SMSCore { to   :: PhoneNumber,
            from :: PhoneNumber,
            body :: Text }
  deriving (Show, Eq, Data, Typeable)

-- | A 'TwilioMsg' is a record of an SMS sent or received by
-- Twilio. These are returned from calls to the Twilio log API or as
-- responses to new POSTs to the Twilio SMS API.
data SMS =
  SMS { id           :: Id,
        kind         :: SMSKind,
        core         :: SMSCore,
        dateCreated  :: UTCTime,
        dateUpdated  :: UTCTime }
  deriving (Show, Eq, Data, Typeable)

-- Parameter parsing

-- | Indicates that an object can be parsed a Form-Urlencoded data
-- source
class FromFormUrlencoded a where
  fromForm :: [(ByteString, ByteString)] -> Maybe a

-- Aeson JSON Serialization helper types

-- | The UTC format used by Twilio: "%a, %d %b %Y %T %z"
twilioUTCFormat :: String
twilioUTCFormat = "%a, %d %b %Y %T %z"

-- | This has a VERY interesting general type signature, I wonder if
-- it exists somewhere more clearly? 
--
-- > (Monad m, Monad w) => (a -> m b) -> w a -> m (w b)
--
-- Yes! It does! It's Maybe's 'Traversable' instance!
--
-- > (Tranversable t, Applicative f) =>
-- > (a -> f b) -> t a -> f (t b)
maybeParse :: (Value -> Parser a) -> Maybe Value -> Parser (Maybe a)
maybeParse p = maybe (return Nothing) (fmap Just . p)

-- | A super Quick Object creator-helper
qo :: Text -> Text -> Value
qo a b = object [ a .= b ]

-- | Assuming all of the 'Value's are 'Object's, concat the fields
-- using the Monoid instance of the underlying map, thus the concat is
-- left-annihilating (earlier objects have priority)
concatObjects :: [Value] -> Value
concatObjects = Object . mconcat . map unObject
  where unObject (Object m) = m
        unObject _ = error "error: Service.Twilio.Types.concatObjects"

-- | A newtype around UTCTime for serialization of Twilio timestamps
newtype TwilioTime = TwilioTime { unTwilioTime :: UTCTime }

instance FromJSON TwilioTime where
  parseJSON (String s) =
    justZ $ fmap TwilioTime $ parseTime defaultTimeLocale twilioUTCFormat (T.unpack s)
  parseJSON _ = fail "parse Service.Twilio.Types.TwilioTime"

instance ToJSON TwilioTime where
  toJSON (TwilioTime utc) =
    String $ T.pack $ formatTime defaultTimeLocale twilioUTCFormat utc

-- | A newtype around Int for serialization of Twilio prices. It's
-- really disappointing that Twilio didn't just use a JSON double here
newtype USD = Cents { unCents :: Int }

instance FromJSON USD where
  parseJSON (String s) =
    fmap (Cents . negate . round . (*100))
    $ rightZ $ A.parseOnly (A.signed A.double) s
  parseJSON _ = fail "parse Service.Twilio.Types.USD"

instance ToJSON USD where
  toJSON (Cents p) =
    String $ T.pack $ printf "%.2f\n" (negate $ (/100) $ fromIntegral p :: Float)

instance FromJSON PhoneNumber where
  parseJSON (String s) = pure . fromString . T.unpack $ s
  parseJSON _ = fail "parse Service.Twilio.Types.PhoneNumber"

instance ToJSON PhoneNumber where
  toJSON (PhoneNumber p) =
    case B.length p of
         10 -> String (TE.decodeUtf8 $ "+1" <> p)
         11 -> String (TE.decodeUtf8 $ "+" <> p)
         _  -> String (TE.decodeUtf8 p)

versionFromString :: (Eq a, IsString a) => a -> Maybe APIVersion
versionFromString "2010-04-01" = Just Api20100401
versionFromString _            = Nothing

instance FromJSON APIVersion where
  parseJSON (String s) = justZ (versionFromString s)
  parseJSON _ = fail "parse Service.Twilio.Types.APIVersion"

instance ToJSON APIVersion where
  toJSON = String . fromString . show

instance FromJSON Id where
  parseJSON (Object o) =
    Id <$> o .: "account_sid"
       <*> o .: "api_version"
       <*> o .: "sid"
  parseJSON _ = fail "parse Service.Twilio.Types.Id"

-- | Creates a partial Twilio JSON object, reconstituting
-- "account_sid", "api_version", "sid", and "uri".
instance ToJSON Id where
  toJSON id@(Id { .. }) =
    object [ "account_sid" .= account,
             "api_version" .= version,
             "sid"         .= sid,
             "uri"         .= url ]
    where url = uri "SMS/Messages" id <> ".json"

instance FromFormUrlencoded Id where
  fromForm ps = do accountSid <- lookup "AccountSid" ps
                   sid        <- lookup "SmsSid" ps <|> lookup "CallSid" ps
                   -- This always fails through to the default
                   -- instance, but it's nice to have theoretically
                   version    <- (lookup "ApiVersion" ps >>= versionFromString)
                                 <|> Just def
                   return Id { version = version,
                               account = accountSid,
                               sid     = sid }

instance FromJSON APIKind where
  parseJSON (Object o) = do
    d <- o .: "direction" :: Parser String
    case d of
      "outbound-api"   -> pure API
      "outbound-reply" -> pure Reply
      _                -> fail "parse Service.Twilio.Types.APIKind"
  parseJSON _ = fail "parse Service.Twilio.Types.APIKind not an object"

instance ToJSON APIKind where
  toJSON API   = qo "direction" "outbound-api"
  toJSON Reply = qo "direction" "outbound-reply"
  
instance FromJSON SendStatus where
  parseJSON (Object o) = do
    d <- o .: "direction" :: Parser String
    case d of
      "outbound-api"   -> doStatus
      "outbound-reply" -> doStatus
      _                -> fail "parse Service.Twilio.Types.SendStatus"
      where doStatus =
              do status <- o .: "status" :: Parser String
                 case status of
                   "queued"  -> pure Queued
                   "sending" -> pure Sending
                   "failed"  -> pure Failed
                   "sent"    ->
                     Sent <$> fmap unTwilioTime (o .: "date_sent")
                          <*> fmap unCents (o .:  "price")
                   _         ->
                     fail "parse Service.Twilio.Types.SendStatus invalid status"
  parseJSON _ = fail "parse Service.Twilio.Types.SendStatus not an object"

instance ToJSON SendStatus where
  toJSON Queued  = qo "status" "queued"
  toJSON Sending = qo "status" "sending"
  toJSON Failed  = qo "status" "failed"
  toJSON (Sent ts price) = object [ "status"    .= T.pack "sent",
                                    "date_sent" .= TwilioTime ts,
                                    "price"     .= Cents price ]

instance FromJSON SMSKind where
  parseJSON obj@(Object o) = do
    direction   <- o .: "direction" :: Parser String
    case direction of
      "inbound" -> pure Inbound
      "outbound-reply" -> rest
      "outbound-api"   -> rest
      _                -> fail "parse Service.Twilio.Types.SMSKind invalid direction"
      where rest = Outbound <$> parseJSON obj <*> parseJSON obj
  parseJSON _ = fail "parse Service.Twilio.Types.SMSKind not an object"

instance ToJSON SMSKind where
  toJSON Inbound = qo "direction" "inbound"
  toJSON (Outbound api status) = concatObjects [ toJSON api, toJSON status ]

instance FromJSON SMSCore where
  parseJSON (Object o) = do
    to          <- o .: "to"
    from        <- o .: "from"
    body        <- o .: "body"
    return SMSCore { to = to, from = from, body = body }
  parseJSON _ = fail "parse Service.Twilio.Types.SMSCore"

instance ToJSON SMSCore where
  toJSON (SMSCore { ..}) =
    object [ "to" .= to, "from" .= from, "body" .= body ]

instance FromFormUrlencoded SMSCore where
  fromForm ps = do to   <- lookup "To" ps
                   from <- lookup "From" ps
                   body <- lookup "Body" ps
                   return SMSCore { to   = fromString $ BC.unpack to,
                                    from = fromString $ BC.unpack from,
                                    body = TE.decodeUtf8 body }

instance FromJSON SMS where
  parseJSON obj@(Object o) = do
    id          <- parseJSON obj
    kind        <- parseJSON obj
    core        <- parseJSON obj
    dateCreated <- o .: "date_created"
    dateUpdated <- o .: "date_updated"

    return SMS { id   = id,
                 kind = kind,
                 core = core,
                 dateCreated = unTwilioTime dateCreated,
                 dateUpdated = unTwilioTime dateUpdated }
  parseJSON _ = fail "parse Service.Twilio.Types.SMS not an object"

instance ToJSON SMS where
  toJSON (SMS { .. }) =
    concatObjects [ toJSON id,
                    toJSON kind,
                    toJSON core,
                    object ["date_created" .= TwilioTime dateCreated],
                    object ["date_updated" .= TwilioTime dateUpdated]
                  ]

-- Arbitrary Instances

-- | A helper for generating arbitrary base-16 strings
genB16 :: Int -> Gen ByteString
genB16 = fmap (B16.encode . B.pack) . vector

instance Arbitrary Passport where
  arbitrary = do
    sid <- genB16 16
    token <- genB16 16
    return $ Passport ("AC" <> sid) token

-- | Creates arbitrary US-based phone number with country codes
instance Arbitrary PhoneNumber where
  arbitrary = fmap (fromString . ('1':) . map intToDigit)
                   (vectorOf 9 $ choose (0,9))

-- | Here we assume that SMS always cost 10 cents
instance Arbitrary SendStatus where
  arbitrary = oneof [pure Queued,
                     pure Sending,
                     Sent <$> arbitrary <*> pure 10,
                     pure Failed]

instance Arbitrary APIKind where
  arbitrary = elements [ API, Reply ]

instance Arbitrary SMSKind where
  arbitrary = oneof [ Outbound <$> arbitrary <*> arbitrary,
                      pure Inbound ]

-- | Assumes the API version is fixed as "2010-04-01"
instance Arbitrary Id where
  arbitrary = do
    acct <- fmap ("AC" <>) (genB16 16)
    sid  <- fmap ("SM" <>) (genB16 16)
    return Id { account = acct,
                sid     = sid,
                version = Api20100401 }

-- | The random body generation should create semi-meaningful body
-- text obeying the size limit.
instance Arbitrary SMSCore where
  arbitrary =
    do body <- fmap T.pack arbitraryBody
       to   <- arbitrary
       from <- arbitrary
       return SMSCore { to = to, from = from, body = body }
    where
      arbitraryBody = do n <- choose (20, 160)
                         vectorOf n $ frequency [(1, pure ' '),
                                                 (3, choose ('a', 'z'))]


-- | Creates an arbitrary SMS guaranteeing that the 'dateCreated' and
-- 'dateUpdated' and 'dateSent' fields are sensible.
instance Arbitrary SMS where
  arbitrary = do
    dateCreated  <- arbitrary
    dateUpdated  <- arbitrary `suchThat` (> dateCreated)
    kind         <- fmap (mkValidDate dateUpdated) arbitrary
    core         <- arbitrary
    id           <- arbitrary
    return SMS { kind = kind, core = core, id = id,
                 dateCreated = dateCreated,
                 dateUpdated = dateUpdated }
    where mkValidDate d (Outbound a (Sent _ p)) =
            Outbound a (Sent d p)
          mkValidDate _ k = k
