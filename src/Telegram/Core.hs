{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Core where

import Data.Function ( (&) )
import Control.Monad ( replicateM_ )
import Bot ( MonadBot, react, evalBotT, InMessage(..), OutMessage(..) )
import qualified Bot ( Config )
import Logger
import Telegram.BotTypes
import Telegram.Api

sendOutMessage :: (MonadApi m, MonadLogger m) => UserId -> OutMessage MessageId -> m ()
sendOutMessage userId outMessage = do
  case outMessage of
    SendText text ->
      sendMessage userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard userId text buttons

handleUpdate :: (MonadApi m, MonadBot MessageId m, MonadLogger m) => Update -> m ()
handleUpdate update = do
  logLn Info (show update)
  case update of
    UnknownUpdate {} ->
      logLn Info $ "Ignoring update: " ++ show update
    Update { uUserId, uEvent } ->
      case uEvent of
        EventMessage (MessageWithId mesId message) -> do
          outMessage <- case message of
            TextMessage { tmText } ->
              react (InTextMessage tmText mesId)
            MediaMessage ->
              react (InMediaMessage mesId)
          sendOutMessage uUserId outMessage
        CallbackQuery { cqId, cqData } -> do
          outMessage <- react (KeyboardKeyPushed cqData)
          sendOutMessage uUserId outMessage
          answerCallbackQuery cqId

botLoop :: (MonadApi m, MonadBot MessageId m, MonadLogger m) => UpdateId -> m ()
botLoop offset = do
  logLn Info $ "Offset: " ++ show offset
  eitherUpdates <- getUpdates offset
  case eitherUpdates of
    Left err      -> logLn Error err
    Right []      -> botLoop offset
    Right updates -> do
      mapM_ handleUpdate updates
      let newOffset = updates & last & uId & unUpdateId & (+1) & UpdateId
      botLoop newOffset

runBot :: Level -> Bot.Config -> Token -> UpdateId -> IO ()
runBot logLevel botConfig token offset =
  botLoop offset
  & flip evalBotT botConfig
  & flip runConsoleLoggerT logLevel
  & flip runApi token
