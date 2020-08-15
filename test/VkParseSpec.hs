{-# LANGUAGE OverloadedStrings #-}

module VkParseSpec where

import Test.Hspec
import Data.Aeson ( decode )
import VkTypes

spec :: Spec
spec = do
  describe "parser" $ do
    it "understands result" $ do
      let res = decode "{\"ts\": \"4\", \"updates\": [{\"type\": \"message_new\", \"object\": {\"id\": 5, \"date\": 1597522977, \"out\": 0, \"user_id\": 1248374, \"read_state\": 0, \"title\": \"\", \"body\": \"42\", \"owner_ids\": []}, \"group_id\": 12345566, \"event_id\": \"h44827ab\"}]}" :: Maybe Result
      res `shouldBe`
        Just
        ( Result
          { rTs = "4"
          , rUpdates =
            [ Update
              { uType = "message_new"
              , uObject = Message
                          { mId = 5
                          , mBody = "42"
                          }
              }
            ]
          }
        )
