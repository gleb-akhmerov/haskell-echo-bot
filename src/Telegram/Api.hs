{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telegram.Api where

import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad.State ( State )
import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Data.Aeson ( object, (.=), toJSONList )
import Telegram.BotTypes
import Util ( requestJSON, verboseEitherDecode )

data Token = Token String deriving Show

class Monad m => MonadTelegram m where
  getUpdates :: Token -> UpdateId -> m (Either String [Update])
  sendMessage :: Token -> UserId -> String -> m ()
  forwardMessage :: Token -> UserId -> MessageId -> m ()
  sendKeyboard :: Token -> UserId -> String -> [Int] -> m ()
  answerCallbackQuery :: Token -> CallbackQueryId -> m ()

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadTelegram m
  ) => MonadTelegram (t m) where
  getUpdates token updateId = lift (getUpdates token updateId)
  sendMessage token userId text = lift (sendMessage token userId text)
  forwardMessage token userId messageId = lift (forwardMessage token userId messageId)
  sendKeyboard token userId text buttons = lift (sendKeyboard token userId text buttons)
  answerCallbackQuery token queryId = lift (answerCallbackQuery token queryId)

instance MonadTelegram IO where
  getUpdates (Token token) (UpdateId offset) = do
    response <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/getUpdates")
        (object [ "offset" .= offset
                , "timeout" .= (25 :: Integer)
                ])
    let json = getResponseBody response
    return $ verboseEitherDecode json >>= parseResult

  sendMessage (Token token) (UserId userId) text = do
    _ <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
        (object ["chat_id" .= userId, "text" .= text])
    return ()

  forwardMessage (Token token) (UserId userId) (MessageId messageId) = do
    _ <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/forwardMessage")
        (object [ "chat_id" .= userId
                , "from_chat_id" .= userId
                , "message_id" .= messageId
                ])
    return ()

  sendKeyboard (Token token) (UserId userId) text buttons = do
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

  answerCallbackQuery (Token token) (CallbackQueryId queryId) = do
    _ <- httpLBS $
      requestJSON
        ("https://api.telegram.org/bot" ++ token ++ "/answerCallbackQuery")
        (object ["callback_query_id" .= queryId])
    return ()

newtype TelegramMock a
  = TelegramMock { tmState :: State () a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadTelegram TelegramMock where
  getUpdates _ updateId = return $ Right []
  sendMessage _ userId text = return ()
  forwardMessage _ userId messageId = return ()
  sendKeyboard _ userId text buttons = return ()
  answerCallbackQuery _ queryId = return ()
