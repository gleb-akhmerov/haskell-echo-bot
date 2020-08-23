{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Core where

import Data.Function ( (&) )
import Control.Monad ( replicateM_ )
import Control.Monad.State ( StateT, liftIO )
import Bot
import Telegram.BotTypes
import Telegram.Api

sendOutMessage :: Token -> UserId -> OutMessage MessageId -> IO ()
sendOutMessage token userId outMessage =
  case outMessage of
    SendText text ->
      sendMessage token userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage token userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: Config -> Token -> Update -> StateT Int IO ()
handleUpdate config token update =
  case update of
    UnknownUpdate {} ->
      liftIO $ putStrLn $ "Ignoring update: " ++ show update
    Update { uUserId, uEvent } ->
      case uEvent of
        EventMessage (MessageWithId mesId message) -> do
          outMessage <- case message of
            TextMessage { tmText } ->
              react config (InTextMessage tmText mesId)
            MediaMessage ->
              react config (InMediaMessage mesId)
          liftIO $ sendOutMessage token uUserId outMessage
        CallbackQuery { cqId, cqData } -> do
          outMessage <- react config (KeyboardKeyPushed cqData)
          liftIO $ sendOutMessage token uUserId outMessage
          liftIO $ answerCallbackQuery token cqId

handleUpdates :: Config -> Token -> [Update] -> StateT Int IO ()
handleUpdates config token updates =
  case updates of
    [] -> return ()
    (u:us) -> do
      handleUpdate  config token u
      handleUpdates config token us

runBot :: Config -> Token -> UpdateId -> StateT Int IO ()
runBot config token offset = do
  liftIO $ putStrLn $ "Offset: " ++ show offset
  eitherUpdates <- liftIO $ getUpdates token offset
  case eitherUpdates of
    Left err      -> liftIO $ putStrLn err
    Right []      -> runBot config token offset
    Right updates -> do
      handleUpdates config token updates
      let newOffset = updates & last & uId & unUpdateId & (+1) & UpdateId
      runBot config token newOffset
