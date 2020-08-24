{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Vk.Api where

import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Util ( requestQuery, verboseEitherDecode )
import Data.Aeson ( encode, object, (.=) )
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Random ( randomIO )
import Data.Traversable ( for )
import Vk.Types

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
