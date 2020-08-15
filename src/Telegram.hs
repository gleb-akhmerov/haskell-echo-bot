{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram where

import Data.Function ( (&) )
import Control.Monad ( forM_ )
import Control.Monad.State ( StateT, liftIO )
import Network.HTTP.Simple ( httpLBS, setRequestBodyJSON, getResponseBody, parseRequest_, setRequestMethod, Request )
import Data.Aeson ( decode, Value(..), object, (.=), toJSONList, eitherDecode )
import Bot
import TelegramBotTypes
import qualified TelegramTypes as T
import Data.ByteString.Lazy.Char8 ( unpack )
import Data.Either ( fromLeft )
import Prelude hiding ( id )

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
        Left $ "Unable to parse json from getUpdates:\n" ++ (unpack json) ++ "\n" ++ fromLeft "wtf" (eitherDecode json :: Either String (T.Result [T.Update]))
      Just r ->
        parseResult r

sendMessage :: String -> Integer -> String -> IO ()
sendMessage token chatId text = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      (object ["chat_id" .= chatId, "text" .= text])
  return ()

forwardMessage :: String -> Integer -> Integer -> IO ()
forwardMessage token chatId messageId = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/forwardMessage")
      (object [ "chat_id" .= chatId
              , "from_chat_id" .= chatId
              , "message_id" .= messageId
              ])
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

sendOutMessage :: String -> Integer -> OutMessage Integer -> IO ()
sendOutMessage token userId outMessage =
  case outMessage of
    SendText text ->
      sendMessage token userId text
    EchoTimes n messageId ->
      forM_ (replicate n messageId) (forwardMessage token userId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: String -> Update -> Config -> StateT Int IO ()
handleUpdate _ (u @ UnknownUpdate {}) _ =
  liftIO $ putStrLn $ "Ignoring update: " ++ show u
handleUpdate token (Update { uUserId, uEvent }) config =
  case uEvent of
    EventMessage (MessageWithId id message) -> do
      outMessage <- case message of
        TextMessage { tmText } ->
          react config (InTextMessage tmText id)
        MediaMessage ->
          react config (InMediaMessage id)
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
    Left err      -> liftIO $ putStrLn err
    Right []      -> runBot token offset config
    Right updates -> do
      handleUpdates token updates config
      let newOffset = updates & last & uId & (+1)
      runBot token newOffset config
