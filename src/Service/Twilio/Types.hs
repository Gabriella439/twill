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
newtype PhoneNumber = PhoneNumber ByteString
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

-- | The type of API used to send an outgoing message
data APIKind = API | Reply deriving (Show, Eq, Data, Typeable)

-- | The type of SMS, be it incoming or outgoing
data SMSKind = Outbound APIKind SendStatus | Inbound
             deriving (Show, Eq, Data, Typeable)

-- | A unique identifier for a message based on the account SID, API
-- version, and message SID. This can be used to construct the
-- canonical Twilio API URI.
data Id = Id { account :: ByteString, version :: ByteString, sid :: ByteString }
        deriving (Show, Eq, Data, Typeable)

-- | Create a URI derived from an 'Id'
uri :: ByteString -> Id -> ByteString
uri typ (Id { .. }) = "/" <> version
                      <> "/Accounts/" <> account
                      <> "/" <> typ <> "/"
                      <> sid

-- | A 'TwilioMsg' is a record of an SMS sent or received by
-- Twilio. These are returned from calls to the Twilio log API or as
-- responses to new POSTs to the Twilio SMS API.
data SMS =
  SMS { id           :: Id,

        kind         :: SMSKind,
        body         :: Text,
        localNumber  :: PhoneNumber,
        distalNumber :: PhoneNumber,
        
        dateCreated  :: UTCTime,
        dateUpdated  :: UTCTime }
  deriving (Show, Eq, Data, Typeable)

-- Aeson JSON Serialization helper types

-- | The UTC format used by Twilio: "%a, %d %b %Y %T %z"
twilioUTCFormat :: String
twilioUTCFormat = "%a, %d %b %Y %T %z"

-- | This has a VERY interesting general type signature, I wonder if
-- it exists somewhere more clearly?
--
-- > (Monad m, Monad w) => (a -> m b) -> w a -> m (w a)
--
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


-- Actual, exportable Aeson instances
        
instance FromJSON SMS where
  parseJSON obj@(Object o) = do
    id          <- parseJSON obj
    kind        <- parseJSON obj
    body        <- o .: "body"
    dateCreated <- o .: "date_created"
    dateUpdated <- o .: "date_updated"
    to          <- o .: "to"
    from        <- o .: "from"
    let (localNumber, distalNumber) =
          case kind of
            Inbound      -> (to, from)
            Outbound _ _ -> (from, to)

    return SMS { id   = id,
                 kind = kind,
                 body = body,
                 localNumber = localNumber,
                 distalNumber = distalNumber,
                 dateCreated = unTwilioTime dateCreated,
                 dateUpdated = unTwilioTime dateUpdated }
  parseJSON _ = fail "parse Service.Twilio.Types.SMS not an object"

instance ToJSON SMS where
  toJSON (SMS { .. }) =
    concatObjects [ toJSON id,
                    toJSON kind,
                    qo "body" body,
                    object ["date_created" .= TwilioTime dateCreated],
                    object ["date_updated" .= TwilioTime dateUpdated],
                    toFrom ]
    where
      toFrom :: Value
      toFrom =
        case kind of
          Inbound -> object [ "to"   .= localNumber,
                              "from" .= distalNumber ]
          _       -> object [ "from" .= localNumber,
                              "to"   .= distalNumber ]

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
                version = "2010-04-01" }

-- | Creates an arbitrary SMS guaranteeing that the 'dateCreated' and
-- 'dateUpdated' and 'dateSent' fields are sensible. The random body
-- generation should create semi-meaningful body text obeying the size
-- limit.
instance Arbitrary SMS where
  arbitrary = do
    dateCreated  <- arbitrary
    dateUpdated  <- arbitrary `suchThat` (> dateCreated)
    kind         <- fmap (mkValidDate dateUpdated) arbitrary
    body         <- fmap T.pack arbitraryBody
    id           <- arbitrary
    localNumber  <- arbitrary
    distalNumber <- arbitrary
    return SMS { kind = kind, body = body, id = id,
                 localNumber = localNumber,
                 distalNumber = distalNumber,
                 dateCreated = dateCreated,
                 dateUpdated = dateUpdated }
    where mkValidDate d (Outbound a (Sent _ p)) =
            Outbound a (Sent d p)
          mkValidDate _ k = k
          arbitraryBody = do n <- choose (20, 160)
                             vectorOf n $ frequency [(1, pure ' '),
                                                     (3, choose ('a', 'z'))]