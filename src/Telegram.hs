{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram where

import Data.Function ( (&) )
import Control.Monad ( forM_ )
import Control.Monad.State ( StateT, liftIO )
import Network.HTTP.Simple ( httpLBS, setRequestBodyJSON, getResponseBody, parseRequest_, setRequestMethod, Request )
import Data.Aeson ( decode, Value(..), object, (.=), toJSONList )
import Bot
import TelegramBotTypes
import qualified TelegramTypes as T
import Data.Text ( pack )
import Data.Char ( toLower )

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
      (object [ "offset" .= offset
              , "timeout" .= (60 :: Integer)
              ])
  let json = getResponseBody response
  let maybeResult = decode json :: Maybe (T.Result [T.Update])
  return $
    case maybeResult of
      Nothing ->
        Left $ "Unable to parse json from getUpdates: " ++ (show json)
      Just r ->
        parseResult r

sendMessage :: String -> Integer -> String -> IO ()
sendMessage token chatId text = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      (object ["chat_id" .= chatId, "text" .= text])
  return ()

sendMedia :: String -> Integer -> Media -> IO ()
sendMedia token chatId (Media { mType, mFileId }) = do
  let typeName = show mType
  let paramName = map toLower typeName
  let method = "/send" ++ typeName
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ method)
      (object ["chat_id" .= chatId, pack paramName .= mFileId])
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

answerCallbackQuery :: String -> String -> IO ()
answerCallbackQuery token queryId = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/answerCallbackQuery")
      (object ["callback_query_id" .= queryId])
  return ()

sendOutMessage :: String -> Integer -> OutMessage Message -> IO ()
sendOutMessage token userId outMessage =
  case outMessage of
    SendText text ->
      sendMessage token userId text
    EchoTimes n message ->
      case message of
        TextMessage { tmText } ->
          forM_ (replicate n tmText) (sendMessage token userId)
        MediaMessage media ->
          forM_ (replicate n media) (sendMedia token userId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: String -> Update -> Config -> StateT Int IO ()
handleUpdate _ (u @ UnknownUpdate {}) _ =
  liftIO $ putStrLn $ "Ignoring update: " ++ show u
handleUpdate token (Update { uUserId, uEvent }) config =
  case uEvent of
    EventMessage message -> do
      outMessage <- case message of
        TextMessage { tmText } ->
          react config (InTextMessage tmText message)
        m @ (MediaMessage _) ->
          react config (InMediaMessage m)
      liftIO $ sendOutMessage token uUserId outMessage
    CallbackQuery { cqId, cqData } -> do
      outMessage <- react config (KeyboardKeyPushed cqData)
      liftIO $ sendOutMessage token uUserId outMessage
      liftIO $ answerCallbackQuery token cqId

handleUpdates :: String -> [Update] -> Config -> StateT Int IO ()
handleUpdates token updates config =
  case updates of
    [] -> return ()
    (u:us) -> do
      handleUpdate token u config
      handleUpdates token us config

runBot :: String -> Integer -> Config -> StateT Int IO ()
runBot token offset config = do
  liftIO $ putStrLn $ "Offset: " ++ show offset
  eitherUpdates <- liftIO $ getUpdates token offset
  case eitherUpdates of
    Left err      -> liftIO $ print err
    Right []      -> runBot token offset config
    Right updates -> do
      handleUpdates token updates config
      let newOffset = updates & last & uId & (+1)
      runBot token newOffset config
