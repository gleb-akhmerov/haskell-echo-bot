{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram where

import Text.Read ( readMaybe )
import Control.Applicative ( empty )
import Data.Function ( (&) )
import Control.Monad ( forM_ )
import Control.Monad.State ( StateT, liftIO )
import Network.HTTP.Simple ( httpLBS, setRequestBodyJSON, getResponseBody, parseRequest_, setRequestMethod, Request )
import Data.Aeson ( FromJSON(..), decode, Value(..), (.:), (.:!), object, (.=), withObject, toJSONList )
import Data.Aeson.Types ( Parser, parseMaybe )
import Bot ( react, InMessage(..), OutMessage(..), Config )

parseResult :: Value -> Parser (Either String [Update])
parseResult = withObject "Result" $ \x -> do
  ok <- x .: "ok"
  case ok of
    True  -> Right <$> x .: "result"
    False -> Left  <$> x .: "description"

data Update
   = Update { updateId :: Integer, updateMessage :: Maybe Message }
   deriving (Show)

data Message
   = TextMessage { messageUserId :: Integer, messageText :: String }
   | CallbackQuery { callbackQueryUserId :: Integer, callbackQueryData :: Int }
   deriving (Show)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \x -> do
    uId         <- x .:  "update_id"
    textMessage <- x .:! "message"
    uMessage <- case textMessage of
      Just m  -> Just <$> parseTextMessage m
      Nothing -> do
        maybeCq <- x .:! "callback_query"
        case maybeCq of
          Just cq -> Just <$> parseCallbackQuery cq
          Nothing -> return Nothing
    return $ Update { updateId = uId, updateMessage = uMessage }

parseTextMessage :: Value -> Parser Message
parseTextMessage = withObject "TextMessage" $ \x -> do
  text   <- x .: "text"
  user   <- x .: "from"
  userId <- user .: "id"
  return $ TextMessage { messageText = text, messageUserId = userId }

parseCallbackQuery :: Value -> Parser Message
parseCallbackQuery = withObject "CallbackQuery" $ \x -> do
  user   <- x .: "from"
  userId <- user .: "id"
  stringData <- x .: "data"
  case readMaybe stringData of
    Nothing -> empty
    Just cqData ->
      return $ CallbackQuery { callbackQueryData = cqData
                             , callbackQueryUserId = userId
                             }

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
  let maybeResult = decode json >>= parseMaybe parseResult
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

sendKeyboard :: String -> Integer -> String -> [Int] -> IO ()
sendKeyboard token userId text buttons = do
  let stringButtons = map show buttons
  let json =
        (object [ "chat_id" .= userId
                , "text" .= text
                , "reply_markup" .=
                  object ["inline_keyboard" .=
                    toJSONList [map (\b -> object ["text" .= b, "callback_data" .= b]) stringButtons]]
                ])
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      json
  return ()      

sendOutMessage :: String -> Integer -> OutMessage -> IO ()
sendOutMessage token userId outMessage =
  case outMessage of
    SendMessageTimes n sendText ->
      forM_ (replicate n sendText) (sendMessage token userId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleMessage :: String -> Config -> Message -> StateT Int IO ()
handleMessage token config message =
  case message of
    TextMessage { messageText, messageUserId } -> do
      outMessage <- react config (InTextMessage messageText)
      liftIO $ sendOutMessage token messageUserId outMessage
    CallbackQuery { callbackQueryData, callbackQueryUserId } -> do
      outMessage <- react config (KeyboardKeyPushed callbackQueryData)
      liftIO $ sendOutMessage token callbackQueryUserId outMessage

handleUpdates :: String -> [Update] -> Config -> StateT Int IO ()
handleUpdates token updates config =
  case updates of
    [] -> return ()
    (u:us) -> case updateMessage u of
      Nothing -> do
        liftIO $ print $ "Ignoring update: " ++ show u
        handleUpdates token us config
      Just message -> do
        handleMessage token config message
        handleUpdates token us config

runBot :: String -> Integer -> Config -> StateT Int IO ()
runBot token offset config = do
  liftIO $ print $ "Offset: " ++ show offset
  eitherUpdates <- liftIO $ getUpdates token offset
  case eitherUpdates of
    Left err      -> liftIO $ print err
    Right []      -> runBot token offset config
    Right updates -> do
      handleUpdates token updates config
      let newOffset = updates & last & updateId & (+1)
      runBot token newOffset config
