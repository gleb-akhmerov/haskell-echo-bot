{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TelegramTypesSpec where

import Test.Hspec
import NeatInterpolation
import Data.Text.Encoding ( encodeUtf8Builder )
import Data.ByteString.Builder ( toLazyByteString )
import Data.Aeson ( decode )
import Telegram.ApiTypes

spec :: Spec
spec = do
  describe "parser" $ do
    it "understands result, updates and text messages" $ do
      let res = decode $ toLazyByteString $ encodeUtf8Builder [text|{
        "ok": true,
        "result": [{
          "update_id": 39473849,
          "message": {
            "message_id": 603,
            "from": {"id": 123456789, "is_bot": false, "first_name": "John", "language_code": "en"},
            "chat": {"id": 123456789, "first_name": "John", "type": "private"},
            "date": 1597290990,
            "text": "/help",
            "entities": [{"offset": 0, "length": 5, "type": "bot_command"}]
          }
        }]
      }|] :: Maybe Result
      res `shouldBe`
        Just
        ( Result
          { rOk = True
          , rDescription = Nothing
          , rResult = Just
            [ Update
              { uUpdateId = 39473849
              , uMessage =
                  Just
                  ( Message
                    { mMessageId = 603
                    , mFrom = User {uId = 123456789}
                    , mText = Just "/help"
                    }
                  )
              , uCallbackQuery = Nothing
              }
            ]
          }
        )
