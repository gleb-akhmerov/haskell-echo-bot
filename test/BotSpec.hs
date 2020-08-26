module BotSpec where

import Test.Hspec
import Control.Monad.State ( runStateT, evalStateT )
import Bot
import Logger

data Mes = Mes deriving (Show, Eq)

spec :: Spec
spec = do
  describe "Bot.react" $ do
    it "echoes any non-command message n times, n is configurable" $ do
      runLoggerIgnore (evalStateT (react defaultConfig (InTextMessage "text" Mes)) 1)
        `shouldBe` EchoTimes 1 Mes
      runLoggerIgnore (evalStateT (react defaultConfig (InTextMessage "text" Mes)) 2)
        `shouldBe` EchoTimes 2 Mes

    it "replies with the help message when it receives /help command" $ do
      runLoggerIgnore (evalStateT (react defaultConfig (InTextMessage "/help" Mes)) 1)
        `shouldBe` SendText (helpText defaultConfig)

    it "sends the repeat keyboard when it receives /repeat command" $ do
      runLoggerIgnore (evalStateT (react defaultConfig (InTextMessage "/repeat" Mes)) 1)
        `shouldBe` SendKeyboard (repeatKeyboardText defaultConfig) [1, 2, 3, 4, 5]

    it "sets the number of repeats when the user pushes a button on the keyboard" $ do
      runLoggerIgnore (runStateT (react defaultConfig ((KeyboardKeyPushed 5) :: InMessage ())) 1)
        `shouldBe` (SendText "The messages will now be repeated 5 times.", 5)

      let conversation = do
           _ <- react defaultConfig (KeyboardKeyPushed 5 :: InMessage ())
           react defaultConfig (InTextMessage "text" Mes)
      runLoggerIgnore (evalStateT conversation 1) `shouldBe` (EchoTimes 5 Mes)
