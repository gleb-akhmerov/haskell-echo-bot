{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module TelegramTypes where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON(..), camelTo2, fieldLabelModifier, genericParseJSON, defaultOptions )

data Result a
   = Result
       { rOk :: Bool
       , rDescription :: Maybe String
       , rResult :: Maybe a
       }
   deriving ( Generic, Show, Eq )

data Update
   = Update
       { uUpdateId :: Integer
       , uMessage :: Maybe Message
       , uCallbackQuery :: Maybe CallbackQuery
       }
   deriving ( Generic, Show, Eq )

data CallbackQuery
   = CallbackQuery
       { cqId :: String
       , cqFrom :: User
       , cqData :: String
       }
   deriving ( Generic, Show, Eq )

data Message
   = Message
       { mFrom :: User
       , mText :: Maybe String
       }
   deriving ( Generic, Show, Eq )

data User
   = User { uId :: Integer }
   deriving ( Generic, Show, Eq )

instance FromJSON a => FromJSON (Result a) where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON CallbackQuery where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 2 }

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }
