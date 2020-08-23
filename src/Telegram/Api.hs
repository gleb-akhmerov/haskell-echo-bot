{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Api where

import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Data.Aeson ( object, (.=), toJSONList )
import Telegram.BotTypes
import Util ( requestJSON, verboseEitherDecode )
import Control.Monad.IO.Class ( MonadIO )

data Token = Token String

getUpdates :: MonadIO m => Token -> UpdateId -> m (Either String [Update])
getUpdates (Token token) (UpdateId offset) = do
  response <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/getUpdates")
      (object [ "offset" .= offset
              , "timeout" .= (25 :: Integer)
              ])
  let json = getResponseBody response
  return $ verboseEitherDecode json >>= parseResult

sendMessage :: MonadIO m => Token -> UserId -> String -> m ()
sendMessage (Token token) (UserId userId) text = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      (object ["chat_id" .= userId, "text" .= text])
  return ()

forwardMessage :: MonadIO m => Token -> UserId -> MessageId -> m ()
forwardMessage (Token token) (UserId userId) (MessageId messageId) = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/forwardMessage")
      (object [ "chat_id" .= userId
              , "from_chat_id" .= userId
              , "message_id" .= messageId
              ])
  return ()

sendKeyboard :: MonadIO m => Token -> UserId -> String -> [Int] -> m ()
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

answerCallbackQuery :: MonadIO m => Token -> CallbackQueryId -> m ()
answerCallbackQuery (Token token) (CallbackQueryId queryId) = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/answerCallbackQuery")
      (object ["callback_query_id" .= queryId])
  return ()
