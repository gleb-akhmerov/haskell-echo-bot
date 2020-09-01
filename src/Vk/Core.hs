{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Vk.Core where

import Data.Function ( (&) )
import Control.Monad.Reader ( MonadReader, asks, runReaderT )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad ( replicateM_ )
import Bot
import Logger
import Vk.Types
import Vk.Api

data VkConfig
   = VkConfig
       { vcToken :: Token
       , vcGroupId :: Integer
       , vcBotConfig :: Config
       }
  deriving (Show)

sendOutMessage :: (MonadIO m, MonadReader VkConfig m, MonadLogger m) => Integer -> OutMessage Integer -> m ()
sendOutMessage userId outMessage = do
  token <- asks vcToken
  case outMessage of
    SendText text ->
      sendTextMessage token userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage token userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: (MonadIO m, MonadReader VkConfig m, MonadBot Integer m, MonadLogger m) => Update -> m ()
handleUpdate (Update { uObject }) = do
  case uObject of
    Message { mId, mUserId, mText = "" } -> do
      outMessage <- react (InMediaMessage mId)
      sendOutMessage mUserId outMessage
    Message { mUserId, mPayload = Just payload } -> do
      outMessage <- react (KeyboardKeyPushed payload)
      sendOutMessage mUserId outMessage
    Message { mId, mUserId, mText } -> do
      outMessage <- react (InTextMessage mText mId)
      sendOutMessage mUserId outMessage
    UnknownObject ->
      return ()

botLoop :: (MonadIO m, MonadReader VkConfig m, MonadBot Integer m, MonadLogger m) => String -> String -> String -> m ()
botLoop server key ts = do
  eitherUpdates <- getUpdates server key ts
  case eitherUpdates of
    Left err ->
      logLn Error err
    Right (Result { rTs, rUpdates }) -> do
      logLn Debug $ show rUpdates
      mapM_ handleUpdate rUpdates
      botLoop server key rTs

startPolling :: (MonadIO m, MonadReader VkConfig m, MonadBot Integer m, MonadLogger m) => m ()
startPolling = do
  groupId <- asks vcGroupId
  token <- asks vcToken
  eitherResponse <- getLongPollServer token groupId
  case eitherResponse of
    Left err ->
      logLn Error err
    Right (Response (LongPollServer { lpsKey, lpsServer, lpsTs })) -> do
      botLoop lpsServer lpsKey lpsTs
  return ()

runBot :: Level -> VkConfig -> IO ()
runBot logLevel config =
  startPolling
  & flip runReaderT config
  & flip evalBotT (vcBotConfig config)
  & flip runConsoleLoggerT logLevel
