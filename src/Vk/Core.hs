{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Vk.Core where

import Data.Functor ( (<&>) )
import Control.Monad.State ( MonadState, evalStateT )
import Control.Monad.Reader ( MonadReader, ask, runReaderT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad ( replicateM_ )
import Bot
import Vk.Types
import Vk.Api

data VkConfig
   = VkConfig
       { vcToken :: Token
       , vcGroupId :: Integer
       , vcBotConfig :: Config
       }

sendOutMessage :: (MonadIO m, MonadReader VkConfig m) => Integer -> OutMessage Integer -> m ()
sendOutMessage userId outMessage = do
  token <- ask <&> vcToken
  case outMessage of
    SendText text ->
      sendTextMessage token userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage token userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: (MonadIO m, MonadReader VkConfig m, MonadState Int m) => Update -> m ()
handleUpdate (Update { uObject }) = do
  config <- ask <&> vcBotConfig
  case uObject of
    Message { mId, mUserId, mText = "" } -> do
      outMessage <- react config (InMediaMessage mId)
      sendOutMessage mUserId outMessage
    Message { mUserId, mPayload = Just payload } -> do
      outMessage <- react config (KeyboardKeyPushed payload)
      sendOutMessage mUserId outMessage
    Message { mId, mUserId, mText } -> do
      outMessage <- react config (InTextMessage mText mId)
      sendOutMessage mUserId outMessage
    UnknownObject ->
      return ()

handleUpdates :: (MonadIO m, MonadReader VkConfig m, MonadState Int m) => [Update] -> m ()
handleUpdates [] = return ()
handleUpdates (u:us) = do
  handleUpdate  u
  handleUpdates us

botLoop :: (MonadIO m, MonadReader VkConfig m, MonadState Int m) => String -> String -> String -> m ()
botLoop server key ts = do
  eitherUpdates <- getUpdates server key ts
  case eitherUpdates of
    Left err ->
      liftIO $ putStrLn err
    Right (Result { rTs, rUpdates }) -> do
      liftIO $ print rUpdates
      handleUpdates rUpdates
      botLoop server key rTs

startPolling :: (MonadIO m, MonadReader VkConfig m, MonadState Int m) => m ()
startPolling = do
  groupId <- ask <&> vcGroupId
  token <- ask <&> vcToken
  eitherResponse <- getLongPollServer token groupId
  case eitherResponse of
    Left err ->
      liftIO $ putStrLn err
    Right (Response (LongPollServer { lpsKey, lpsServer, lpsTs })) -> do
      botLoop lpsServer lpsKey lpsTs
  return ()

runBot :: VkConfig -> Int -> IO ()
runBot config repeats =
  evalStateT (runReaderT startPolling config) repeats
