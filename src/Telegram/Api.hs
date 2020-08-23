{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Api where

import Network.HTTP.Simple ( httpLBS, getResponseBody )
import Data.Aeson ( object, (.=), toJSONList )
import Telegram.BotTypes
import Util ( requestJSON, verboseEitherDecode )

data Token = Token String

getUpdates :: Token -> UpdateId -> IO (Either String [Update])
getUpdates (Token token) (UpdateId offset) = do
  response <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/getUpdates")
      (object [ "offset" .= offset
              , "timeout" .= (25 :: Integer)
              ])
  let json = getResponseBody response
  return $ verboseEitherDecode json >>= parseResult

sendMessage :: Token -> UserId -> String -> IO ()
sendMessage (Token token) (UserId userId) text = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/sendMessage")
      (object ["chat_id" .= userId, "text" .= text])
  return ()

forwardMessage :: Token -> UserId -> MessageId -> IO ()
forwardMessage (Token token) (UserId userId) (MessageId messageId) = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/forwardMessage")
      (object [ "chat_id" .= userId
              , "from_chat_id" .= userId
              , "message_id" .= messageId
              ])
  return ()

sendKeyboard :: Token -> UserId -> String -> [Int] -> IO ()
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

answerCallbackQuery :: Token -> CallbackQueryId -> IO ()
answerCallbackQuery (Token token) (CallbackQueryId queryId) = do
  _ <- httpLBS $
    requestJSON
      ("https://api.telegram.org/bot" ++ token ++ "/answerCallbackQuery")
      (object ["callback_query_id" .= queryId])
  return ()
