{-# LANGUAGE NamedFieldPuns #-}

module Telegram.BotTypes where

import Data.Function ( (&) )
import qualified Telegram.ApiTypes as T

newtype UpdateId = UpdateId { unUpdateId :: Integer } deriving ( Eq, Show )
newtype UserId = UserId { unUserId :: Integer } deriving ( Eq, Show )
newtype CallbackQueryId = CallbackQueryId { unCallbackQueryId :: String } deriving ( Eq, Show )
newtype MessageId = MessageId { unMessageId :: Integer } deriving ( Eq, Show )

data Update
   = Update
       { uId :: UpdateId
       , uUserId :: UserId
       , uEvent :: Event
       }
   | UnknownUpdate
       { uId :: UpdateId }
   deriving ( Show )

data Event
   = EventMessage MessageWithId
   | CallbackQuery
       { cqId :: CallbackQueryId
       , cqData :: Int
       }
   deriving ( Show )

data MessageWithId
   = MessageWithId MessageId Message
   deriving ( Show )

data Message
   = TextMessage { tmText :: String }
   | MediaMessage
   deriving ( Show )

parseResult :: T.Result -> Either String [Update]
parseResult T.Result { T.rOk = True, T.rResult = Just r } =
  Right $ map parseUpdate r
parseResult T.Result { T.rOk = False, T.rDescription = Just d } =
  Left d
parseResult result =
  Left $ "Can't parse: " ++ show result

parseUpdate :: T.Update -> Update
parseUpdate T.Update { T.uUpdateId, T.uMessage = Just m } =
  case parseMessage m of
    Left _ ->
      UnknownUpdate { uId = UpdateId uUpdateId }
    Right message ->
      Update { uId = UpdateId uUpdateId
             , uEvent = EventMessage $
                          MessageWithId (MessageId (T.mMessageId m)) message
             , uUserId = UserId (m & T.mFrom & T.uId)
             }
parseUpdate T.Update { T.uUpdateId, T.uCallbackQuery = Just cq } =
  Update { uId = UpdateId uUpdateId
         , uEvent = parseCallbackQuery cq
         , uUserId = UserId (cq & T.cqFrom & T.uId)
         }
parseUpdate T.Update { T.uUpdateId } =
  UnknownUpdate { uId = UpdateId uUpdateId }

parseMessage :: T.Message -> Either String Message
parseMessage T.Message { T.mText = Just t } =
  Right TextMessage { tmText = t }
parseMessage _ =
  Right $ MediaMessage

parseCallbackQuery :: T.CallbackQuery -> Event
parseCallbackQuery cq =
  CallbackQuery { cqId = CallbackQueryId (T.cqId cq)
                , cqData = cq & T.cqData & read
                }
