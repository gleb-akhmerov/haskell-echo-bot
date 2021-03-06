{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.Types where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON(..), camelTo2, fieldLabelModifier, genericParseJSON, defaultOptions, (.:), (.:!), withObject )
import Data.Aeson.Types ( Parser, Value )

data Response
   = Response { rResponse :: LongPollServer }
   deriving ( Generic, Show, Eq )

data LongPollServer
   = LongPollServer
       { lpsKey :: String
       , lpsServer :: String
       , lpsTs :: String
       }
   deriving ( Generic, Show, Eq )

data Result
   = Result
       { rTs :: String
       , rUpdates :: [Update]
       }
   deriving ( Generic, Show, Eq )

data Update
   = Update
       { uType :: String
       , uObject :: Object
       }
   deriving ( Show, Eq )

data Object
   = Message
       { mId :: Integer
       , mText :: String
       , mUserId :: Integer
       , mPayload :: Maybe Int
       }
   | UnknownObject
   deriving ( Show, Eq )

instance FromJSON Response where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON LongPollServer where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 3 }

instance FromJSON Result where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON Update where
  parseJSON = withObject "Update" $ \o -> do
    uType <- o .: "type"
    uObject <- case uType of
      "message_new" ->
        o .: "object" >>= parseMessage
      _ ->
        return UnknownObject
    return $ Update {..}

parseMessage :: Value -> Parser Object
parseMessage = withObject "Message" $ \o -> do
  mes <- o .: "message"
  Message <$> mes .:  "id"
          <*> mes .:  "text"
          <*> mes .:  "from_id"
          <*> do payload <- mes .:! "payload"
                 return $ fmap read payload
