{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telegram.Api where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson ( object, (.=), toJSONList, Value, encode )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( MonadReader, ReaderT, ask, runReaderT )
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Telegram.BotTypes
import Util ( requestJSON, verboseEitherDecode )
import Logger

data Token = Token String deriving (Show, Eq)

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

newtype ApiT m a
  = Api { unApi :: ReaderT String m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

runApiT :: ApiT m a -> Token -> m a
runApiT (Api m) (Token token) = runReaderT m token

callApi :: (MonadIO m, MonadLogger m, MonadReader String m) => String -> Value -> m LBS.ByteString
callApi method payload = do
  token <- ask
  logLn Debug $ method ++ " " ++ LBS.unpack (encode payload)
  resp <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/" ++ method)
      payload
  let body = getResponseBody resp
  logLn Debug $ "response: " ++ LBS.unpack body
  return body

instance (MonadLogger m, MonadIO m) => MonadApi (ApiT m) where
  getUpdates (UpdateId offset) = Api $ do
    json <- callApi "getUpdates"
              (object [ "offset" .= offset
                      , "timeout" .= (25 :: Integer)
                      ])
    return $ verboseEitherDecode json >>= parseResult

  sendMessage (UserId userId) text = Api $ do
    _ <- callApi "sendMessage"
           (object ["chat_id" .= userId, "text" .= text])
    return ()

  forwardMessage (UserId userId) (MessageId messageId) = Api $ do
    _ <- callApi "forwardMessage"
           (object [ "chat_id" .= userId
                   , "from_chat_id" .= userId
                   , "message_id" .= messageId
                   ])
    return ()

  sendKeyboard (UserId userId) text buttons = Api $ do
    let stringButtons = map show buttons
    _ <- callApi "sendMessage"
           (object [ "chat_id" .= userId
                   , "text" .= text
                   , "reply_markup" .=
                     object ["inline_keyboard" .=
                             toJSONList [map (\b -> object ["text" .= b, "callback_data" .= b]) stringButtons]]
                   ])
    return ()

  answerCallbackQuery (CallbackQueryId queryId) = Api $ do
    _ <- callApi "answerCallbackQuery"
           (object ["callback_query_id" .= queryId])
    return ()
