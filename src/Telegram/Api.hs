{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telegram.Api where

import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad.State
import Control.Monad.Reader
import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Data.Aeson ( object, (.=), toJSONList )
import Telegram.BotTypes
import Util ( requestJSON, verboseEitherDecode )

data Token = Token String deriving Show

class Monad m => MonadApi m where
  getUpdates :: UpdateId -> m (Either String [Update])
  sendMessage :: UserId -> String -> m ()
  forwardMessage :: UserId -> MessageId -> m ()
  sendKeyboard :: UserId -> String -> [Int] -> m ()
  answerCallbackQuery :: CallbackQueryId -> m ()

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadApi m
  ) => MonadApi (t m) where
  getUpdates updateId = lift (getUpdates updateId)
  sendMessage userId text = lift (sendMessage userId text)
  forwardMessage userId messageId = lift (forwardMessage userId messageId)
  sendKeyboard userId text buttons = lift (sendKeyboard userId text buttons)
  answerCallbackQuery queryId = lift (answerCallbackQuery queryId)

newtype Api a
  = Api { unApi :: ReaderT String IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadApi Api where
  getUpdates (UpdateId offset) = Api $ do
    token <- ask
    response <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/getUpdates")
        (object [ "offset" .= offset
                , "timeout" .= (25 :: Integer)
                ])
    let json = getResponseBody response
    return $ verboseEitherDecode json >>= parseResult

  sendMessage (UserId userId) text = Api $ do
    token <- ask
    _ <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
        (object ["chat_id" .= userId, "text" .= text])
    return ()

  forwardMessage (UserId userId) (MessageId messageId) = Api $ do
    token <- ask
    _ <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/forwardMessage")
        (object [ "chat_id" .= userId
                , "from_chat_id" .= userId
                , "message_id" .= messageId
                ])
    return ()

  sendKeyboard (UserId userId) text buttons = Api $ do
    token <- ask
    let stringButtons = map show buttons
    _ <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
        (object [ "chat_id" .= userId
                , "text" .= text
                , "reply_markup" .=
                  object ["inline_keyboard" .=
                          toJSONList [map (\b -> object ["text" .= b, "callback_data" .= b]) stringButtons]]
                ])
    return ()

  answerCallbackQuery (CallbackQueryId queryId) = Api $ do
    token <- ask
    _ <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/answerCallbackQuery")
        (object ["callback_query_id" .= queryId])
    return ()

runApi :: Api a -> Token -> IO a
runApi (Api m) (Token token) = runReaderT m token


newtype Mock a
  = Mock { tmState :: State () a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadApi Mock where
  getUpdates updateId = return $ Right []
  sendMessage userId text = return ()
  forwardMessage userId messageId = return ()
  sendKeyboard userId text buttons = return ()
  answerCallbackQuery queryId = return ()
