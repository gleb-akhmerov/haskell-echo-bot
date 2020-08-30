{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Core where

import Data.Function ( (&) )
import Control.Monad ( replicateM_ )
import Control.Monad.Reader ( MonadReader, asks, runReaderT )
import Bot
import Logger
import Telegram.BotTypes
import Telegram.Api

data TelegramConfig
   = TelegramConfig
       { tcToken :: Token
       , tcBotConfig :: Config
       }
  deriving (Show)

sendOutMessage :: (MonadReader TelegramConfig m, MonadTelegram m, MonadLogger m) => UserId -> OutMessage MessageId -> m ()
sendOutMessage userId outMessage = do
  token <- asks tcToken
  case outMessage of
    SendText text ->
      sendMessage token userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage token userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: (MonadReader TelegramConfig m, MonadTelegram m, MonadBot MessageId m, MonadLogger m) => Update -> m ()
handleUpdate update =
  case update of
    UnknownUpdate {} ->
      logLn Info $ "Ignoring update: " ++ show update
    Update { uUserId, uEvent } -> do
      config <- asks tcBotConfig
      case uEvent of
        EventMessage (MessageWithId mesId message) -> do
          outMessage <- case message of
            TextMessage { tmText } ->
              react config (InTextMessage tmText mesId)
            MediaMessage ->
              react config (InMediaMessage mesId)
          sendOutMessage uUserId outMessage
        CallbackQuery { cqId, cqData } -> do
          outMessage <- react config (KeyboardKeyPushed cqData)
          sendOutMessage uUserId outMessage
          token <- asks tcToken
          answerCallbackQuery token cqId

botLoop :: (MonadReader TelegramConfig m, MonadTelegram m, MonadBot MessageId m, MonadLogger m) => UpdateId -> m ()
botLoop offset = do
  token <- asks tcToken
  logLn Info $ "Offset: " ++ show offset
  eitherUpdates <- getUpdates token offset
  case eitherUpdates of
    Left err      -> logLn Error err
    Right []      -> botLoop offset
    Right updates -> do
      mapM_ handleUpdate updates
      let newOffset = updates & last & uId & unUpdateId & (+1) & UpdateId
      botLoop newOffset

runBot :: Level -> TelegramConfig -> UpdateId -> IO ()
runBot logLevel config offset =
  let repeats = config & tcBotConfig & initialRepeats
  in runConsoleLoggerT (evalBotT (runReaderT (botLoop offset) config) repeats) logLevel
