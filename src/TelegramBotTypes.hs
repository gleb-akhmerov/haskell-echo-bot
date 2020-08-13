{-# LANGUAGE NamedFieldPuns #-}

module TelegramBotTypes where

import Data.Function ( (&) )
import qualified TelegramTypes as T

data Update
   = Update
       { uId :: Integer
       , uUserId :: Integer
       , uEvent :: Event
       }
   | UnknownUpdate
       { uId :: Integer }
   deriving ( Show )

data Event
   = EventMessage Message
   | CallbackQuery
       { cqId :: String
       , cqData :: Int
       }
   deriving ( Show )

data Message
   = TextMessage { tmText :: String }
   | Animation { aFileId :: String }
   deriving ( Show )

parseResult :: T.Result [T.Update] -> Either String [Update]
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
      UnknownUpdate { uId = uUpdateId }
    Right message ->
      Update { uId = uUpdateId
             , uEvent = EventMessage message
             , uUserId = m & T.mFrom & T.uId
             }
parseUpdate T.Update { T.uUpdateId, T.uCallbackQuery = Just cq } =
  Update { uId = uUpdateId
         , uEvent = parseCallbackQuery cq
         , uUserId = cq & T.cqFrom & T.uId
         }
parseUpdate T.Update { T.uUpdateId } =
  UnknownUpdate { uId = uUpdateId }

parseMessage :: T.Message -> Either String Message
parseMessage T.Message { T.mText = Just t } =
  Right TextMessage { tmText = t }
parseMessage T.Message { T.mAnimation = Just a } =
  Right Animation { aFileId = T.aFileId a }
parseMessage m @ (T.Message _ Nothing Nothing) =
  Left $ "Can't parse: " ++ show m

parseCallbackQuery :: T.CallbackQuery -> Event
parseCallbackQuery cq =
  CallbackQuery { cqId = T.cqId cq, cqData = cq & T.cqData & read }
