{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, NamedFieldPuns #-}

-- | Just re-exports a few of the types from
-- "Service.Twilio.Types". There are a number of helper functions
-- which are currently exposed by "Service.Twilio.Types" that aren't
-- exposed here.
module Service.Twilio (
  Price,
  Passport (..),
  PhoneNumber (..),
  SendStatus (..),
  APIKind (..),
  SMSKind (..),
  Id (..), uri,
  SMS (..)
  ) where

import Prelude hiding (id)
import Service.Twilio.Types