module BotSpec where

import Test.Hspec
import Bot
import Logger

data Mes = Mes deriving (Show, Eq)

spec :: Spec
spec = do
  describe "Bot.react" $ do
    let config = Config { helpText = "Help text"
                        , repeatKeyboardText = "Repeat keyboard text"
                        , initialRepeats = 1
                        }

    it "echoes any non-command message n times, n is configurable" $ do
      runNoLogging (evalBotT (react (InTextMessage "text" Mes)) config { initialRepeats = 1 })
        `shouldBe` EchoTimes 1 Mes
      runNoLogging (evalBotT (react (InTextMessage "text" Mes)) config { initialRepeats = 2 })
        `shouldBe` EchoTimes 2 Mes

    it "replies with the help message when it receives /help command" $ do
      runNoLogging (evalBotT (react (InTextMessage "/help" Mes)) config)
        `shouldBe` SendText (helpText config)

    it "sends the repeat keyboard when it receives /repeat command" $ do
      runNoLogging (evalBotT (react (InTextMessage "/repeat" Mes)) config)
        `shouldBe` SendKeyboard (repeatKeyboardText config) [1, 2, 3, 4, 5]

    it "sets the number of repeats when the user pushes a button on the keyboard" $ do
      runNoLogging (runBotT (react ((KeyboardKeyPushed 5) :: InMessage ())) config)
        `shouldBe` (SendText "The messages will now be repeated 5 times.", 5)

      let conversation = do
           _ <- react (KeyboardKeyPushed 5 :: InMessage ())
           react (InTextMessage "text" Mes)
      runNoLogging (evalBotT conversation config) `shouldBe` (EchoTimes 5 Mes)
