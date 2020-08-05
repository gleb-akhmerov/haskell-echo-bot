import Test.Hspec
import Bot (Action(..), Config(..), InMessage(..), OutMessage(..), defaultConfig, messageToAction, react)

main = hspec spec

spec :: Spec
spec = do
  describe "Bot.react" $ do
    it "echoes any non-command message n times, n is configurable" $ do
      react (defaultConfig { repeats = 1 }) (InTextMessage "text")
        `shouldBe` [OutTextMessage "text"]
      react (defaultConfig { repeats = 2 }) (InTextMessage "text")
        `shouldBe` [OutTextMessage "text", OutTextMessage "text"]
    it "replies with the help message when it receives /help command" $ do
      react defaultConfig (InTextMessage "/help")
        `shouldBe` [OutTextMessage (helpText defaultConfig)]

  describe "Bot.messageToAction" $ do
    it "separates commands and regular messages" $ do
      messageToAction (InTextMessage "Some text") `shouldBe` Echo "Some text"
      messageToAction (InTextMessage "/help") `shouldBe` Help
