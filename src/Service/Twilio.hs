{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, NamedFieldPuns #-}

-- | Just re-exports a few of the types from
-- "Service.Twilio.Types". There are a number of helper functions
-- which are currently exposed by "Service.Twilio.Types" that aren't
-- exposed here.
module Service.Twilio (
  -- * Base Twilio types
  Price, -- type synonym for 'Int'
  Passport (..),
  PhoneNumber (..),
  SendStatus (..),
  APIKind (..),
  SMSKind (..),
  Id (..), uri,
  SMS (..),
  -- * Request signing
  requestSignature,
  requestSignature'
  ) where

import Prelude hiding (id)
import Service.Twilio.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Monoid
import Data.List
import Data.Ord

import Crypto.Hash.SHA1 (hash)
import Crypto.MAC.HMAC (hmac)


-- | Given a Passport, a target URL, the raw query string, and a set
-- of body parameters, this function computes the canonical request
-- signature Twilio uses to authenticate itself.
requestSignature :: Passport
                    -> ByteString -- ^ The full URL
                    -> ByteString -- ^ The raw query string including the "?"
                    -> [(ByteString, ByteString)] -- ^ Post parameters in Body
                    -> ByteString
requestSignature = requestSignature' False False

-- | A more flexible form of 'requestSignature' intended to be used
-- with the API inconsistencies for HTTP call requests and HTTPS call
-- requests. See the bottom of <http://www.twilio.com/docs/security>
-- for more details.
requestSignature' :: Passport
                     -> Bool -- ^ Should the username/password be dropped?
                     -> Bool -- ^ Should the port be dropped?
                     -> ByteString -- ^ The full URL
                     -> ByteString -- ^ The raw query string including the "?"
                     -> [(ByteString, ByteString)] -- ^ Post parameters in Body
                     -> ByteString
requestSignature' (Passport _ token) url qs headers =
  encode $ url <> qs <> canonize headers
  where encode = B64.encode . hmac hash 64 token
        canonize = mconcat . map (uncurry mappend) . sortBy (comparing fst)

