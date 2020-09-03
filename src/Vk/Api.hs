{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vk.Api where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader ( ReaderT, runReaderT, ask )
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Traversable ( for )
import System.Random ( randomIO )
import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Data.Aeson ( encode, object, (.=) )
import Util ( requestQuery, verboseEitherDecode )
import Vk.Types
import Logger

data Token = Token String deriving (Show, Eq)

class Monad m => MonadApi m where
  getLongPollServer :: Integer -> m (Either String Response)
  getUpdates :: String -> String -> String -> m (Either String Result)
  sendTextMessage :: Integer -> String -> m ()
  sendKeyboard :: Integer -> String -> [Int] -> m ()
  forwardMessage :: Integer -> Integer -> m ()

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadApi m
  ) => MonadApi (t m) where
  getLongPollServer groupId = lift (getLongPollServer groupId)
  getUpdates server key ts = lift (getUpdates server key ts)
  sendTextMessage userId text = lift (sendTextMessage userId text)
  sendKeyboard userId text buttons = lift (sendKeyboard userId text buttons)
  forwardMessage userId messageId = lift (forwardMessage userId messageId)

newtype ApiT m a
  = ApiT { unApiT :: ReaderT String m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

runApiT :: ApiT m a -> Token -> m a
runApiT (ApiT m) (Token token) = runReaderT m token

instance (Monad m, MonadLogger m, MonadIO m) => MonadApi (ApiT m) where
  getLongPollServer groupId = ApiT $ do
    token <- ask
    response <- httpLBS $
      requestQuery
        "https://api.vk.com/method/groups.getLongPollServer"
        [ ("v", "5.120")
        , ("access_token", token)
        , ("group_id", show groupId)
        ]
    return $ verboseEitherDecode $ getResponseBody response

  getUpdates server key ts = ApiT $ do
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

  sendTextMessage userId text = ApiT $ do
    token <- ask
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

  sendKeyboard userId text buttons = ApiT $ do
    token <- ask
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

  forwardMessage userId messageId = ApiT $ do
    token <- ask
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
