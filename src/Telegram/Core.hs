{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Core where

import Data.Function ( (&) )
import Control.Monad ( replicateM_ )
import Control.Monad.State ( StateT, liftIO )
import Bot
import Telegram.BotTypes
import Telegram.Api
import Prelude hiding ( id )

sendOutMessage :: String -> UserId -> OutMessage MessageId -> IO ()
sendOutMessage token userId outMessage =
  case outMessage of
    SendText text ->
      sendMessage token userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage token userId messageId)
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
