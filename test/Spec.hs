import Test.Hspec
import Control.Monad.State.Lazy (runState, evalState)
import Bot (Config(..), InMessage(..), OutMessage(..), defaultConfig, react)

main = hspec spec

spec :: Spec
spec = do
  describe "Bot.react" $ do
    it "echoes any non-command message n times, n is configurable" $ do
      evalState (react $ InTextMessage "text") (defaultConfig { repeats = 1 })
        `shouldBe` SendMessageTimes 1 "text"
      evalState (react $ InTextMessage "text") (defaultConfig { repeats = 2 })
        `shouldBe` SendMessageTimes 2 "text"

    it "replies with the help message when it receives /help command" $ do
      evalState (react $ InTextMessage "/help") defaultConfig
        `shouldBe` SendMessageTimes 1 (helpText defaultConfig)

    it "sends the repeat keyboard when it receives /repeat command" $ do
      evalState (react $ InTextMessage "/repeat") defaultConfig
        `shouldBe` SendKeyboard (repeatKeyboardText defaultConfig) [1, 2, 3, 4, 5]

    it "sets the number of repeats when the user pushes a button on the keyboard" $ do
      runState (react $ KeyboardKeyPushed 5) defaultConfig
        `shouldBe` (SendMessageTimes 1 "The messages will now be repeated 5 times.", defaultConfig { repeats = 5 })
