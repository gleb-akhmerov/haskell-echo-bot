import Test.Hspec
import Control.Monad.State.Lazy (runState, evalState)
import Bot (Config(..), InMessage(..), OutMessage(..), defaultConfig, react)

main = hspec spec

spec :: Spec
spec = do
  describe "Bot.react" $ do
    it "echoes any non-command message n times, n is configurable" $ do
      evalState (react defaultConfig (InTextMessage "text")) 1
        `shouldBe` SendMessageTimes 1 "text"
      evalState (react defaultConfig (InTextMessage "text")) 2
        `shouldBe` SendMessageTimes 2 "text"

    it "replies with the help message when it receives /help command" $ do
      evalState (react defaultConfig (InTextMessage "/help")) 1
        `shouldBe` SendMessageTimes 1 (helpText defaultConfig)

    it "sends the repeat keyboard when it receives /repeat command" $ do
      evalState (react defaultConfig (InTextMessage "/repeat")) 1
        `shouldBe` SendKeyboard (repeatKeyboardText defaultConfig) [1, 2, 3, 4, 5]

    it "sets the number of repeats when the user pushes a button on the keyboard" $ do
      runState (react defaultConfig (KeyboardKeyPushed 5)) 1
        `shouldBe` (SendMessageTimes 1 "The messages will now be repeated 5 times.", 5)
