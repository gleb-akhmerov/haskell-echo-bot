{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram where

import Data.Function ( (&) )
import Control.Monad ( forM_ )
import Control.Monad.State ( StateT, liftIO )
import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Data.Aeson ( decode, object, (.=), toJSONList, eitherDecode )
import Bot
import TelegramBotTypes
import qualified TelegramTypes as T
import Data.ByteString.Lazy.Char8 ( unpack )
import Data.Either ( fromLeft )
import Prelude hiding ( id )
import Util ( requestJSON )

getUpdates :: String -> UpdateId -> IO (Either String [Update])
getUpdates token (UpdateId offset) = do
  response <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/getUpdates")
      (object [ "offset" .= offset
              , "timeout" .= (60 :: Integer)
              ])
  let json = getResponseBody response
  let maybeResult = decode json :: Maybe T.Result
  return $
    case maybeResult of
      Nothing ->
        Left $ "Unable to parse json from getUpdates:\n" ++ (unpack json) ++ "\n" ++ fromLeft "wtf" (eitherDecode json :: Either String T.Result)
      Just r ->
        parseResult r

sendMessage :: String -> UserId -> String -> IO ()
sendMessage token (UserId userId) text = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      (object ["chat_id" .= userId, "text" .= text])
  return ()

forwardMessage :: String -> UserId -> MessageId -> IO ()
forwardMessage token (UserId userId) (MessageId messageId) = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/forwardMessage")
      (object [ "chat_id" .= userId
              , "from_chat_id" .= userId
              , "message_id" .= messageId
              ])
  return ()

sendKeyboard :: String -> UserId -> String -> [Int] -> IO ()
sendKeyboard token (UserId userId) text buttons = do
  let stringButtons = map show buttons
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      (object [ "chat_id" .= userId
              , "text" .= text
              , "reply_markup" .=
                object ["inline_keyboard" .=
                        toJSONList [map (\b -> object ["text" .= b, "callback_data" .= b]) stringButtons]]
              ])
  return ()      

answerCallbackQuery :: String -> CallbackQueryId -> IO ()
answerCallbackQuery token (CallbackQueryId queryId) = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/answerCallbackQuery")
      (object ["callback_query_id" .= queryId])
  return ()

sendOutMessage :: String -> UserId -> OutMessage MessageId -> IO ()
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

runBot :: String -> UpdateId -> Config -> StateT Int IO ()
runBot token offset config = do
  liftIO $ putStrLn $ "Offset: " ++ show offset
  eitherUpdates <- liftIO $ getUpdates token offset
  case eitherUpdates of
    Left err      -> liftIO $ putStrLn err
    Right []      -> runBot token offset config
    Right updates -> do
      handleUpdates token updates config
      let newOffset = updates & last & uId & unUpdateId & (+1) & UpdateId
      runBot token newOffset config
