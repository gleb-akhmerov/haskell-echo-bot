{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram where

import           Network.HTTP.Simple ( httpLBS, getResponseBody, parseRequest_ )
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Aeson ( FromJSON(..), decode, genericParseJSON, defaultOptions, fieldLabelModifier, camelTo2 )
import           GHC.Generics ( Generic )

data Result a = Result
  { ok :: Bool
  , result :: Maybe a
  } deriving (Generic, Show)


data Update = Update
  { updateId :: Integer
  , message :: Maybe Message
  } deriving (Generic, Show)

data Message = Message
  { from :: User
  , text :: Maybe String
  } deriving (Generic, Show)

data User = User
  { id :: Integer
  } deriving (Generic, Show)

instance (FromJSON a) => FromJSON (Result a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runBot :: String -> IO ()
runBot token = do
  res <- httpLBS $ parseRequest_ $ "https://api.telegram.org/bot" ++ token ++ "/getUpdates"
  let json = getResponseBody res
  LBS.putStrLn json
  print (decode json :: Maybe (Result [Update]))
