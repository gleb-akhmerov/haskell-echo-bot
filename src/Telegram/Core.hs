{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Core where

import Data.Function ( (&) )
import Control.Monad ( replicateM_ )
import Control.Monad.State ( MonadState, evalStateT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader ( MonadReader, asks, runReaderT )
import Bot ( react, Config, InMessage(..), OutMessage(..) )
import Telegram.BotTypes
import Telegram.Api

data TelegramConfig
   = TelegramConfig
       { tcToken :: Token
       , tcBotConfig :: Config
       }

sendOutMessage :: (MonadReader TelegramConfig m, MonadIO m) => UserId -> OutMessage MessageId -> m ()
sendOutMessage userId outMessage = do
  token <- asks tcToken
  case outMessage of
    SendText text ->
      sendMessage token userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage token userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: (MonadReader TelegramConfig m, MonadIO m, MonadState Int m) => Update -> m ()
handleUpdate update =
  case update of
    UnknownUpdate {} ->
      liftIO $ putStrLn $ "Ignoring update: " ++ show update
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

botLoop :: (MonadReader TelegramConfig m, MonadIO m, MonadState Int m) => UpdateId -> m ()
botLoop offset = do
  token <- asks tcToken
  liftIO $ putStrLn $ "Offset: " ++ show offset
  eitherUpdates <- getUpdates token offset
  case eitherUpdates of
    Left err      -> liftIO $ putStrLn err
    Right []      -> botLoop offset
    Right updates -> do
      mapM_ handleUpdate updates
      let newOffset = updates & last & uId & unUpdateId & (+1) & UpdateId
      botLoop newOffset

runBot :: TelegramConfig -> Int -> UpdateId -> IO ()
runBot config repeats offset =
  evalStateT (runReaderT (botLoop offset) config) repeats
