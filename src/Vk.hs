{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Vk where

import Network.HTTP.Simple ( httpLBS, getResponseBody )
import VkTypes
import Util ( requestQuery, verboseEitherDecode )
import Bot
import Control.Monad.State ( runState )
import Control.Monad ( replicateM_ )
import Data.Aeson ( encode, object, (.=) )
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Random ( randomIO )
import Data.Traversable ( for )

getLongPollServer :: String -> Integer -> IO (Either String Response)
getLongPollServer token groupId = do
  response <- httpLBS $
    requestQuery
      "https://api.vk.com/method/groups.getLongPollServer"
      [ ("v", "5.120")
      , ("access_token", token)
      , ("group_id", show groupId)
      ]
  return $ verboseEitherDecode $ getResponseBody response

getUpdates :: String -> String -> String -> IO (Either String Result)
getUpdates server key ts = do
  response <- httpLBS $
    requestQuery
      server
      [ ("v", "5.120")
      , ("act", "a_check")
      , ("key", key)
      , ("ts", ts)
      , ("wait", "25")
      ]
  LBS.putStr $ getResponseBody response
  return $ verboseEitherDecode $ getResponseBody response

sendTextMessage :: String -> Integer -> String -> IO ()
sendTextMessage token userId text = do
  randomId <- randomIO :: IO Int
  _ <- httpLBS $
    requestQuery
      "https://api.vk.com/method/messages.send"
      [ ("access_token", token)
      , ("v", "5.120")
      , ("random_id", show randomId)
      , ("peer_id", show userId)
      , ("message", text)
      ]
  return ()

sendKeyboard :: String -> Integer -> String -> [Int] -> IO ()
sendKeyboard token userId text buttons = do
  randomId <- randomIO :: IO Int
  r <- httpLBS $
    requestQuery
      "https://api.vk.com/method/messages.send"
      [ ("access_token", token)
      , ("v", "5.120")
      , ("random_id", show randomId)
      , ("peer_id", show userId)
      , ("message", text)
      , ("keyboard", LBS.unpack $ encode $ object
          [ "one_time" .= True
          , "buttons" .= (for buttons $ \x ->
               [object
                  [ "action"
                    .= object
                         [ "type" .= ("text" :: String)
                         , "payload" .= show x
                         , "label" .= show x
                         ]]])])
      ]
  LBS.putStrLn $ getResponseBody r
  return ()

forwardMessage :: String -> Integer -> Integer -> IO ()
forwardMessage token userId messageId = do
  randomId <- randomIO :: IO Int
  _ <- httpLBS $
    requestQuery
      "https://api.vk.com/method/messages.send"
      [ ("access_token", token)
      , ("v", "5.120")
      , ("random_id", show randomId)
      , ("peer_id", show userId)
      , ("forward_messages", show messageId)
      
      ]
  return ()

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
