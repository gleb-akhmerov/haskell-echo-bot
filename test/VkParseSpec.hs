{-# LANGUAGE OverloadedStrings #-}

module VkParseSpec where

import Test.Hspec
import Util ( verboseEitherDecode )
import Vk.Types

spec :: Spec
spec = do
  describe "parser" $ do
    it "understands result" $ do
      let res = verboseEitherDecode "{\"ts\": \"4\", \"updates\": [{\"type\": \"message_new\", \"object\": {\"message\": {\"date\": 1590000000, \"from_id\": 12, \"id\": 5, \"out\": 0, \"peer_id\": 12, \"text\": \"42\", \"conversation_message_id\": 5, \"fwd_messages\": [], \"important\": false, \"random_id\": 0, \"attachments\": [], \"is_hidden\": false}, \"client_info\": {\"button_actions\": [\"text\", \"vkpay\", \"open_app\", \"location\", \"open_link\", \"callback\"], \"keyboard\": true, \"inline_keyboard\": true, \"carousel\": false, \"lang_id\": 3}}, \"group_id\": 123456789, \"event_id\": \"789abc\"}]}" :: Either String Result
      res `shouldBe`
        Right
        ( Result
          { rTs = "4"
          , rUpdates =
            [ Update
              { uType = "message_new"
              , uObject = Message
                          { mId = 5
                          , mText = "42"
                          , mUserId = 12
                          , mPayload = Nothing
                          }
              }
            ]
          }
        )
