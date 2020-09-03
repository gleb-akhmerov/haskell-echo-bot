{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec where

import Test.Hspec
import NeatInterpolation
import Data.Ini.Config ( parseIniFile )
import qualified Bot ( Config(..) )
import qualified Telegram.Api as Tg ( Token(..) )
import qualified Vk ( Config(..) )
import qualified Vk.Api as Vk ( Token(..) )
import Config ( Config(..), Api(..), configParser )
import Logger ( Level(..) )

spec :: Spec
spec = do
  describe "configParser" $ do
    it "parses config" $ do
      let config = [text|
        [Bot]
        help_text = Help text.
        repeat_keyboard_text = Repeat keyboard text.
        initial_repeats = 1

        [Telegram]
        token = 789abc

        [VK]
        token = 123def
        group_id = 4242

        [Run]
        api = Telegram
        log_level = Debug
        |]
      parseIniFile config configParser
        `shouldBe`
          (Right $
            Config
              { cTelegramToken = Tg.Token "789abc"
              , cBot = Bot.Config
                         { Bot.helpText = "Help text."
                         , Bot.repeatKeyboardText = "Repeat keyboard text."
                         , Bot.initialRepeats = 1 }
              , cVk = Vk.Config 
                        { Vk.cToken = Vk.Token "123def"
                        , Vk.cGroupId = 4242 }
              , cApi = Telegram
              , cLogLevel = Debug })
