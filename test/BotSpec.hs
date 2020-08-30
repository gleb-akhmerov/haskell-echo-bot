module BotSpec where

import Test.Hspec
import Control.Monad.State ( runStateT, evalStateT )
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
      runNoLogging (evalStateT (react config (InTextMessage "text" Mes)) 1)
        `shouldBe` EchoTimes 1 Mes
      runNoLogging (evalStateT (react config (InTextMessage "text" Mes)) 2)
        `shouldBe` EchoTimes 2 Mes

    it "replies with the help message when it receives /help command" $ do
      runNoLogging (evalStateT (react config (InTextMessage "/help" Mes)) 1)
        `shouldBe` SendText (helpText config)

    it "sends the repeat keyboard when it receives /repeat command" $ do
      runNoLogging (evalStateT (react config (InTextMessage "/repeat" Mes)) 1)
        `shouldBe` SendKeyboard (repeatKeyboardText config) [1, 2, 3, 4, 5]

    it "sets the number of repeats when the user pushes a button on the keyboard" $ do
      runNoLogging (runStateT (react config ((KeyboardKeyPushed 5) :: InMessage ())) 1)
        `shouldBe` (SendText "The messages will now be repeated 5 times.", 5)

      let conversation = do
           _ <- react config (KeyboardKeyPushed 5 :: InMessage ())
           react config (InTextMessage "text" Mes)
      runNoLogging (evalStateT conversation 1) `shouldBe` (EchoTimes 5 Mes)
