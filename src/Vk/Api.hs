{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Vk.Api where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Traversable ( for )
import System.Random ( randomIO )
import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Data.Aeson ( encode, object, (.=) )
import Util ( requestQuery, verboseEitherDecode )
import Vk.Types
import Logger

data Token = Token String deriving Show

class Monad m => MonadApi m where
  getLongPollServer :: Token -> Integer -> m (Either String Response)
  getUpdates :: String -> String -> String -> m (Either String Result)
  sendTextMessage :: Token -> Integer -> String -> m ()
  sendKeyboard :: Token -> Integer -> String -> [Int] -> m ()
  forwardMessage :: Token -> Integer -> Integer -> m ()

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadApi m
  ) => MonadApi (t m) where
  getLongPollServer token groupId = lift (getLongPollServer token groupId)
  getUpdates server key ts = lift (getUpdates server key ts)
  sendTextMessage token userId text = lift (sendTextMessage token userId text)
  sendKeyboard token userId text buttons = lift (sendKeyboard token userId text buttons)
  forwardMessage token userId messageId = lift (forwardMessage token userId messageId)

newtype ApiT m a
  = ApiT { runApiT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving MonadTrans via IdentityT

instance (Monad m, MonadLogger m, MonadIO m) => MonadApi (ApiT m) where
  getLongPollServer (Token token) groupId = do
    response <- httpLBS $
      requestQuery
        "https://api.vk.com/method/groups.getLongPollServer"
        [ ("v", "5.120")
        , ("access_token", token)
        , ("group_id", show groupId)
        ]
    return $ verboseEitherDecode $ getResponseBody response

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
    logLn Debug $ LBS.unpack $ getResponseBody response
    return $ verboseEitherDecode $ getResponseBody response

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
