{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Core where

import Prelude hiding ( log )
import Data.Function ( (&) )
import Control.Monad ( replicateM_ )
import Control.Monad.State ( MonadState, evalStateT )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( MonadReader, asks, runReaderT )
import Bot ( react, Config, InMessage(..), OutMessage(..), initialRepeats )
import Logger
import Telegram.BotTypes
import Telegram.Api

data TelegramConfig
   = TelegramConfig
       { tcToken :: Token
       , tcBotConfig :: Config
       }
  deriving (Show)

sendOutMessage :: (MonadReader TelegramConfig m, MonadIO m, MonadLogger m) => UserId -> OutMessage MessageId -> m ()
sendOutMessage userId outMessage = do
  token <- asks tcToken
  case outMessage of
    SendText text ->
      sendMessage token userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage token userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: (MonadReader TelegramConfig m, MonadIO m, MonadState Int m, MonadLogger m) => Update -> m ()
handleUpdate update =
  case update of
    UnknownUpdate {} ->
      log Info $ "Ignoring update: " ++ show update
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

botLoop :: (MonadReader TelegramConfig m, MonadIO m, MonadState Int m, MonadLogger m) => UpdateId -> m ()
botLoop offset = do
  token <- asks tcToken
  log Info $ "Offset: " ++ show offset
  eitherUpdates <- getUpdates token offset
  case eitherUpdates of
    Left err      -> log Error err
    Right []      -> botLoop offset
    Right updates -> do
      mapM_ handleUpdate updates
      let newOffset = updates & last & uId & unUpdateId & (+1) & UpdateId
      botLoop newOffset

runBot :: Level -> TelegramConfig -> UpdateId -> IO ()
runBot logLevel config offset =
  let repeats = config & tcBotConfig & initialRepeats
  in runLogger logLevel $ evalStateT (runReaderT (botLoop offset) config) repeats
