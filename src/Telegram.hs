{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram where

import           Data.Function ( (&) )
import           Control.Monad ( forM_ )
import           Control.Monad.State ( StateT, runState, liftIO )
import           Network.HTTP.Simple ( httpLBS, httpNoBody, setRequestBodyJSON, getResponseBody, parseRequest_, setRequestMethod, Request )
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Aeson ( FromJSON(..), decode, genericParseJSON, defaultOptions, fieldLabelModifier, camelTo2, Value ( Bool, Object ), (.:), object, (.=) )
import           Data.Aeson.Types ( Parser, parseMaybe )
import           GHC.Generics ( Generic )
import           Bot ( react, InMessage(..), OutMessage(..), defaultConfig )

parseResult :: FromJSON a => Value -> Parser (Either String a)
parseResult (Object x) = do
  ok <- x .: "ok"
  case ok of
    Bool True  -> Right <$> x .: "result"
    Bool False -> Left  <$> x .: "description"

data Update
   = Update { updateId :: Integer, message :: Maybe Message }
   deriving (Generic, Show)

data Message
   = Message { userId :: Integer, text :: Maybe String }
   deriving (Generic, Show)

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Message where
  parseJSON (Object x) = do
    text   <- x .: "text"
    user   <- x .: "from"
    userId <- user .: "id"
    return $ Message { text = text, userId = userId }

requestJSON :: String -> Value -> Request
requestJSON url json =
  parseRequest_ url
    & setRequestMethod "POST"
    & setRequestBodyJSON json

getUpdates :: String -> Integer-> IO (Either String [Update])
getUpdates token offset = do
  response <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/getUpdates")
      (object ["offset" .= offset])
  let json = getResponseBody response
  let maybeResult = decode json >>= parseMaybe parseResult :: Maybe (Either String [Update])
  return $
    case maybeResult of
      Nothing ->
        Left $ "Unable to parse json as (Result [Update]): " ++ (show json)
      Just res -> res

sendMessage :: String -> Integer -> String -> IO ()
sendMessage token chatId text = do
  httpNoBody $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      (object ["chat_id" .= chatId, "text" .= text])
  return ()

handleMessage :: String -> Message -> StateT Int IO ()
handleMessage token message =
  case text message of
    Nothing -> return ()
    Just text -> do
      outMessage <- react defaultConfig (InTextMessage text)
      case outMessage of
        SendMessageTimes n text ->
          liftIO $ forM_ (replicate n text) (sendMessage token (userId message))
        SendKeyboard text buttons ->
          error "TODO"

handleUpdates :: String -> [Update] -> StateT Int IO ()
handleUpdates token updates =
  case updates of
    [] -> return ()
    (u:us) -> case message u of
      Nothing -> do
        liftIO $ print $ "Ignoring update: " ++ show u
        handleUpdates token us
      Just message -> do
        handleMessage token message
        handleUpdates token us

runBot :: String -> Integer -> StateT Int IO ()
runBot token offset = do
  liftIO $ print $ "Offset: " ++ show offset
  eitherUpdates <- liftIO $ getUpdates token offset
  case eitherUpdates of
    Left error    -> liftIO $ print error
    Right []      -> runBot token offset
    Right updates -> do
      newRepeats <- handleUpdates token updates
      let newOffset = updates & last & updateId & (+1)
      runBot token newOffset
