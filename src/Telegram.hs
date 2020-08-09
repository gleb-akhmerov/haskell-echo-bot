{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram where

import Data.Function ( (&) )
import Control.Monad ( forM_ )
import Control.Monad.State ( StateT, liftIO )
import Network.HTTP.Simple ( httpLBS, setRequestBodyJSON, getResponseBody, parseRequest_, setRequestMethod, Request )
import Data.Aeson ( FromJSON(..), decode, Value, (.:), (.:!), object, (.=), withObject )
import Data.Aeson.Types ( Parser, parseMaybe )
import GHC.Generics ( Generic )
import Bot ( react, InMessage(..), OutMessage(..), defaultConfig )

parseResult :: FromJSON a => Value -> Parser (Either String a)
parseResult = withObject "Result" $ \x -> do
  ok <- x .: "ok"
  case ok of
    True  -> Right <$> x .: "result"
    False -> Left  <$> x .: "description"

data Update
   = Update { updateId :: Integer, updateMessage :: Maybe Message }
   deriving (Generic, Show)

data Message
   = Message { messageUserId :: Integer, messageText :: Maybe String }
   deriving (Generic, Show)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \x -> do
    uId      <- x .:  "update_id"
    uMessage <- x .:! "message"
    return $ Update { updateId = uId, updateMessage = uMessage }

instance FromJSON Message where
  parseJSON = withObject "Message" $ \x -> do
    text   <- x .: "text"
    user   <- x .: "from"
    userId <- user .: "id"
    return $ Message { messageText = text, messageUserId = userId }

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
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      (object ["chat_id" .= chatId, "text" .= text])
  return ()

handleMessage :: String -> Message -> StateT Int IO ()
handleMessage token (Message { messageUserId, messageText }) =
  case messageText of
    Nothing -> return ()
    Just text -> do
      outMessage <- react defaultConfig (InTextMessage text)
      case outMessage of
        SendMessageTimes n sendText ->
          liftIO $ forM_ (replicate n sendText) (sendMessage token messageUserId)
        SendKeyboard _ _ ->
          error "TODO"

handleUpdates :: String -> [Update] -> StateT Int IO ()
handleUpdates token updates =
  case updates of
    [] -> return ()
    (u:us) -> case updateMessage u of
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
    Left err      -> liftIO $ print err
    Right []      -> runBot token offset
    Right updates -> do
      handleUpdates token updates
      let newOffset = updates & last & updateId & (+1)
      runBot token newOffset
