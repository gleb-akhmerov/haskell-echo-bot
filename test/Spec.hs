import Test.Hspec
import Bot (Config(..), InMessage(..), OutMessage(..), defaultConfig, react)

main = hspec spec

spec :: Spec
spec = do
  describe "Bot.react" $ do
    it "echoes any non-command message n times, n is configurable" $ do
      react (defaultConfig { repeats = 1 }) (InTextMessage "text")
        `shouldBe` (defaultConfig { repeats = 1 }, [OutTextMessage "text"])
      react (defaultConfig { repeats = 2 }) (InTextMessage "text")
        `shouldBe` (defaultConfig { repeats = 2 }, [OutTextMessage "text", OutTextMessage "text"])
    it "replies with the help message when it receives /help command" $ do
      react defaultConfig (InTextMessage "/help")
        `shouldBe` (defaultConfig, [OutTextMessage (helpText defaultConfig)])
    it "sends the repeat keyboard when it receives /repeat command" $ do
      react defaultConfig (InTextMessage "/repeat")
        `shouldBe` (defaultConfig, [SendKeyboard (repeatKeyboardText defaultConfig) [1, 2, 3, 4, 5]])
    it "sets the number of repeats when the user pushes a button on the keyboard" $ do
      react defaultConfig (KeyboardKeyPushed 5)
        `shouldBe` (defaultConfig { repeats = 5 }, [OutTextMessage "The messages will now be repeated 5 times."])
