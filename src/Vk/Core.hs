{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Vk.Core where

import Data.Function ( (&) )
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

sendOutMessage :: (MonadApi m, MonadLogger m) => Integer -> OutMessage Integer -> m ()
sendOutMessage userId outMessage =
  case outMessage of
    SendText text ->
      sendTextMessage userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard userId text buttons

handleUpdate :: (MonadApi m, MonadBot Integer m, MonadLogger m) => Update -> m ()
handleUpdate (Update { uObject }) =
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

botLoop :: (MonadApi m, MonadBot Integer m, MonadLogger m) => String -> String -> String -> m ()
botLoop server key ts = do
  eitherUpdates <- getUpdates server key ts
  case eitherUpdates of
    Left err ->
      logLn Error err
    Right (Result { rTs, rUpdates }) -> do
      logLn Debug $ show rUpdates
      mapM_ handleUpdate rUpdates
      botLoop server key rTs

startPolling :: (MonadApi m, MonadBot Integer m, MonadLogger m) => Integer -> m ()
startPolling groupId = do
  eitherResponse <- getLongPollServer groupId
  case eitherResponse of
    Left err ->
      logLn Error err
    Right (Response (LongPollServer { lpsKey, lpsServer, lpsTs })) -> do
      botLoop lpsServer lpsKey lpsTs
  return ()

runBot :: Level -> VkConfig -> IO ()
runBot logLevel config =
  startPolling (vcGroupId config)
  & flip evalBotT (vcBotConfig config)
  & flip runApiT (vcToken config)
  & flip runConsoleLoggerT logLevel
