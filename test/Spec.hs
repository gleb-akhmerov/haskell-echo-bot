import Test.Hspec
import Bot (Action(..), Config(..), Message(..), defaultConfig, messageToAction, react)

main = hspec spec

spec :: Spec
spec = do
  describe "Bot.react" $ do
    it "echoes any non-command message n times, n is configurable" $ do
      let message = Message "Some text"
      react (defaultConfig { repeats = 1 }) message `shouldBe` [message]
      react (defaultConfig { repeats = 2 }) message `shouldBe` [message, message]
    it "replies with the help message when it receives /help command" $ do
      react defaultConfig (Message "/help") `shouldBe` [Message (helpText defaultConfig)]

  describe "Bot.messageToAction" $ do
    it "separates commands and regular messages" $ do
      messageToAction (Message "Some text") `shouldBe` Echo "Some text"
      messageToAction (Message "/help") `shouldBe` Help
