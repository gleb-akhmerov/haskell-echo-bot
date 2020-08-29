{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Vk.Api where

import Prelude hiding ( log )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Traversable ( for )
import System.Random ( randomIO )
import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Data.Aeson ( encode, object, (.=) )
import Util ( requestQuery, verboseEitherDecode )
import Vk.Types
import Logger

data Token = Token String deriving Show

getLongPollServer :: (MonadIO m) => Token -> Integer -> m (Either String Response)
getLongPollServer (Token token) groupId = do
  response <- httpLBS $
    requestQuery
      "https://api.vk.com/method/groups.getLongPollServer"
      [ ("v", "5.120")
      , ("access_token", token)
      , ("group_id", show groupId)
      ]
  return $ verboseEitherDecode $ getResponseBody response

getUpdates :: (MonadIO m, MonadLogger m) => String -> String -> String -> m (Either String Result)
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
  log Debug $ LBS.unpack $ getResponseBody response
  return $ verboseEitherDecode $ getResponseBody response

sendTextMessage :: (MonadIO m) => Token -> Integer -> String -> m ()
sendTextMessage (Token token) userId text = do
  randomId <- liftIO (randomIO :: IO Int)
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

sendKeyboard :: (MonadIO m) => Token -> Integer -> String -> [Int] -> m ()
sendKeyboard (Token token) userId text buttons = do
  randomId <- liftIO (randomIO :: IO Int)
  _ <- httpLBS $
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
  return ()

forwardMessage :: (MonadIO m) => Token -> Integer -> Integer -> m ()
forwardMessage (Token token) userId messageId = do
  randomId <- liftIO (randomIO :: IO Int)
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
