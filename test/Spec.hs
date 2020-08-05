import Test.Hspec
import Bot (react, Config(..), Message(..))

main = hspec spec

spec :: Spec
spec = do
  describe "Bot.react" $ do
    it "echoes any non-command message n times, n is configurable" $ do
      let message = Message { text = "Some text" }
      react (Config { repeats = 1 }) message `shouldBe` [message]
      react (Config { repeats = 2 }) message `shouldBe` [message, message]
