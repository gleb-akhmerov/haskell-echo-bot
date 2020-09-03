{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VkSpec where

import Test.Hspec
import Control.Monad.Writer
import Vk.Api
import Vk.Core
import qualified Vk.Types as Ts
import qualified Bot
import Logger

data OutMessage
  = TextMessage Integer String
  | ForwardedMessage Integer Integer
  | Keyboard Integer String [Int]
  deriving (Eq, Show)

newtype Mock a
  = Mock { unMock :: Writer [OutMessage] a }
  deriving newtype (Functor, Applicative, Monad)

append :: OutMessage -> Mock ()
append x = Mock $ tell [x]

instance MonadApi Mock where
  getLongPollServer _ = error "This shouldn't be called"
  getUpdates _ _ _ = error "This shouldn't be called"
  sendTextMessage userId text = append $ TextMessage userId text
  sendKeyboard userId text buttons = append $ Keyboard userId text buttons
  forwardMessage userId messageId = append $ ForwardedMessage userId messageId

runMock :: Mock a -> [OutMessage]
runMock (Mock m) = execWriter m

makeTextMessageUpdate :: Integer -> Integer -> String -> Ts.Update
makeTextMessageUpdate userId messageId text =
  Ts.Update
    { Ts.uType = "message_new"
    , Ts.uObject = Ts.Message
                     { Ts.mId = messageId
                     , Ts.mText = text
                     , Ts.mUserId = userId
                     , Ts.mPayload = Nothing }}

spec :: Spec
spec = do
  describe "handleUpdate" $ do
    let config = Bot.Config
                   { Bot.helpText = "Help text"
                   , Bot.repeatKeyboardText = "Repeat keyboard text"
                   , Bot.initialRepeats = 1
                   }
    let userId = 123456789
    let messageId = 42

    it "forwards non-command text messages" $ do
      let update = makeTextMessageUpdate userId messageId "Hello!"
      runMock (runNoLoggingT (Bot.evalBotT (handleUpdate update) config))
        `shouldBe` [ForwardedMessage userId messageId]

    it "sends the help text for /help command" $ do
      let update = makeTextMessageUpdate userId messageId "/help"
      runMock (runNoLoggingT (Bot.evalBotT (handleUpdate update) config))
        `shouldBe` [TextMessage userId (Bot.helpText config)]

    it "sends the keyboard for /repeat command" $ do
      let update = makeTextMessageUpdate userId messageId "/repeat"
      runMock (runNoLoggingT (Bot.evalBotT (handleUpdate update) config))
        `shouldBe` [Keyboard userId (Bot.repeatKeyboardText config) [1,2,3,4,5]]
