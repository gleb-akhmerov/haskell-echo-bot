{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Vk where

import Network.HTTP.Simple ( httpLBS, getResponseBody )
import VkTypes
import Util ( requestQuery, verboseEitherDecode )

getLongPollServer :: String -> Integer -> IO (Either String Response)
getLongPollServer token groupId = do
  response <- httpLBS $
    requestQuery
      "https://api.vk.com/method/groups.getLongPollServer"
      [ ("access_token", token)
      , ("v", "5.85")
      , ("group_id", show groupId)
      ]
  return $ verboseEitherDecode $ getResponseBody response

getUpdates :: String -> String -> String -> IO (Either String Result)
getUpdates server key ts = do
  response <- httpLBS $
    requestQuery
      server
      [ ("act", "a_check")
      , ("key", key)
      , ("ts", ts)
      , ("wait", "25")
      ]
  return $ verboseEitherDecode $ getResponseBody response

forwardMessage :: String -> Integer -> Integer -> IO ()
forwardMessage token userId messageId = do
  _ <- httpLBS $
    requestQuery
      "https://api.vk.com/method/messages.send"
      [ ("access_token", token)
      , ("v", "5.85")
      , ("user_id", show userId)
      , ("forward_messages", show messageId)
      ]
  return ()

-- Update {uType = "message_new", uObject = Message {mId = 12, mBody = "3"}}
handleUpdate :: String -> Update -> IO ()
handleUpdate token (Update { uObject }) = do
  case uObject of
    Message { mId, mUserId } ->
      forwardMessage token mUserId mId
    UnknownObject ->
      return ()

runBot' :: String -> String -> String -> String -> IO ()
runBot' token server key ts = do
  eitherUpdates <- getUpdates server key ts
  case eitherUpdates of
    Left err ->
      putStrLn err
    Right (Result { rTs, rUpdates }) -> do
      print rUpdates
      mapM_ (handleUpdate token) rUpdates
      runBot' token server key rTs

runBot :: String -> Integer -> IO ()
runBot token groupId = do
  eitherResponse <- getLongPollServer token groupId
  case eitherResponse of
    Left err ->
      putStrLn err
    Right (Response (LongPollServer { lpsKey, lpsServer, lpsTs })) -> do
      runBot' token lpsServer lpsKey lpsTs
  return ()
