{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TelegramSpec where

import Test.Hspec
import Control.Monad.Writer
import Telegram.Api
import Telegram.Core
import qualified Telegram.BotTypes as BT
import qualified Bot
import Logger

data OutMessage
  = TextMessage BT.UserId String
  | ForwardedMessage BT.UserId BT.MessageId
  | Keyboard BT.UserId String [Int]
  | CallbackQueryAnswer BT.CallbackQueryId
  deriving (Eq, Show)

newtype Mock a
  = Mock { unMock :: Writer [OutMessage] a }
  deriving newtype (Functor, Applicative, Monad)

append :: OutMessage -> Mock ()
append x = Mock $ tell [x]

instance MonadApi Mock where
  getUpdates _ = error "this shouldn't be called"
  sendMessage userId text = append $ TextMessage userId text
  forwardMessage userId messageId = append $ ForwardedMessage userId messageId
  sendKeyboard userId text buttons = append $ Keyboard userId text buttons
  answerCallbackQuery queryId = append $ CallbackQueryAnswer queryId

runMock :: Mock a -> [OutMessage]
runMock (Mock m) = execWriter m

makeTextMessageUpdate :: BT.UserId -> BT.MessageId -> String -> BT.Update
makeTextMessageUpdate userId messageId text =
  BT.Update
  { BT.uId = BT.UpdateId 9999
  , BT.uUserId = userId
  , BT.uEvent = BT.EventMessage
                ( BT.MessageWithId
                    messageId
                    (BT.TextMessage { BT.tmText = text }))}

makeCallbackQueryUpdate :: BT.UserId -> BT.CallbackQueryId -> Int -> BT.Update
makeCallbackQueryUpdate userId cqId cqData =
  BT.Update
  { BT.uId = BT.UpdateId 9999
  , BT.uUserId = userId
  , BT.uEvent = BT.CallbackQuery
             { BT.cqId = cqId
             , BT.cqData = cqData }}

spec :: Spec
spec = do
  describe "handleUpdate" $ do
    let config = Bot.Config { Bot.helpText = "Help text"
                            , Bot.repeatKeyboardText = "Repeat keyboard text"
                            , Bot.initialRepeats = 1
                            }
    let userId = BT.UserId 123456789
    let messageId = BT.MessageId 42

    it "should forward non-command text messages" $ do
      let update = makeTextMessageUpdate userId messageId "Hello!"
      runMock (runNoLoggingT (Bot.evalBotT (handleUpdate update) config))
        `shouldBe` [ForwardedMessage userId messageId]

    it "should send help text for /help command" $ do
      let update = makeTextMessageUpdate userId messageId "/help"
      runMock (runNoLoggingT (Bot.evalBotT (handleUpdate update) config))
        `shouldBe` [TextMessage userId (Bot.helpText config)]

    it "should send the keyboard for /repeat command" $ do
      let update = makeTextMessageUpdate userId messageId "/repeat"
      runMock (runNoLoggingT (Bot.evalBotT (handleUpdate update) config))
        `shouldBe` [Keyboard userId (Bot.repeatKeyboardText config) [1,2,3,4,5]]

    it "should reply to callback query" $ do
      let cqId = BT.CallbackQueryId "123"
      let update = makeCallbackQueryUpdate userId cqId 3
      runMock (runNoLoggingT (Bot.evalBotT (handleUpdate update) config))
        `shouldBe` [ TextMessage userId "The messages will now be repeated 3 times."
                   , CallbackQueryAnswer cqId
                   ]

    it "should reply the requested number of times after a callback query" $ do
      let cqId = BT.CallbackQueryId "123"
          conversation = do
            handleUpdate $ makeTextMessageUpdate userId (BT.MessageId 1) "Hello!"
            handleUpdate $ makeCallbackQueryUpdate userId cqId 3
            handleUpdate $ makeTextMessageUpdate userId (BT.MessageId 2) "Hello!"
      runMock (runNoLoggingT (Bot.evalBotT conversation config))
        `shouldBe` [ ForwardedMessage userId (BT.MessageId 1)
                   , TextMessage userId "The messages will now be repeated 3 times."
                   , CallbackQueryAnswer cqId
                   , ForwardedMessage userId (BT.MessageId 2)
                   , ForwardedMessage userId (BT.MessageId 2)
                   , ForwardedMessage userId (BT.MessageId 2)
                   ]
