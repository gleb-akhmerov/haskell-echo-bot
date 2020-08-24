{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Vk.Core where

import Control.Monad.State ( runState )
import Control.Monad ( replicateM_ )
import Bot
import Vk.Types
import Vk.Api

sendOutMessage :: String -> Integer -> OutMessage Integer -> IO ()
sendOutMessage token userId outMessage = do
  case outMessage of
    SendText text ->
      sendTextMessage token userId text
    EchoTimes n messageId ->
      replicateM_ n (forwardMessage token userId messageId)
    SendKeyboard text buttons ->
      sendKeyboard token userId text buttons

handleUpdate :: Config -> Int -> String -> Update -> IO Int
handleUpdate config repeats token (Update { uObject }) = do
  case uObject of
    Message { mId, mUserId, mText = "" } -> do
      let (outMessage, repeats') = runState (react config (InMediaMessage mId)) repeats
      sendOutMessage token mUserId outMessage
      return repeats'
    Message { mUserId, mPayload = Just payload } -> do
      let (outMessage, repeats') = runState (react config (KeyboardKeyPushed payload)) repeats
      sendOutMessage token mUserId outMessage
      return repeats'
    Message { mId, mUserId, mText } -> do
      let (outMessage, repeats') = runState (react config (InTextMessage mText mId)) repeats
      sendOutMessage token mUserId outMessage
      return repeats'
    UnknownObject ->
      return repeats

handleUpdates :: Config -> Int -> String -> [Update] -> IO Int
handleUpdates config repeats token updates =
  case updates of
    [] ->
      return repeats
    (u:us) -> do
      repeats' <- handleUpdate config repeats token u
      handleUpdates config repeats' token us

runBot' :: Config -> Int -> String -> String -> String -> String -> IO ()
runBot' config repeats token server key ts = do
  eitherUpdates <- getUpdates server key ts
  case eitherUpdates of
    Left err ->
      putStrLn err
    Right (Result { rTs, rUpdates }) -> do
      print rUpdates
      repeats' <- handleUpdates config repeats token rUpdates
      runBot' config repeats' token server key rTs

runBot :: Config -> Int -> String -> Integer -> IO ()
runBot config repeats token groupId = do
  eitherResponse <- getLongPollServer token groupId
  case eitherResponse of
    Left err ->
      putStrLn err
    Right (Response (LongPollServer { lpsKey, lpsServer, lpsTs })) -> do
      runBot' config repeats token lpsServer lpsKey lpsTs
  return ()
