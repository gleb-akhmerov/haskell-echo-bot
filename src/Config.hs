{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import Data.Ini.Config
import qualified Telegram.Api as Tg ( Token(..) )
import qualified Vk.Api as Vk ( Token(..) )
import Vk.Core ( VkConfig(..) )
import Telegram.Core ( TelegramConfig(..) )
import qualified Bot ( Config(..) )
import Logger ( Level )

data Api
  = Telegram
  | VK
  deriving (Read, Show)

data AppConfig
   = AppConfig
       { acTelegram :: TelegramConfig
       , acVk :: VkConfig
       , acApi :: Api
       , acLogLevel :: Level
       }
  deriving Show

configParser :: IniParser AppConfig
configParser = do
  botConfig <- section "Bot" $ do
    helpText <- fieldOf "help_text" string
    repeatKeyboardText <- fieldOf "repeat_keyboard_text" string
    initialRepeats <- fieldOf "initial_repeats" number
    return Bot.Config {..}
  acTelegram <- section "Telegram" $ do
    tcToken <- Tg.Token <$> fieldOf "token" string
    let tcBotConfig = botConfig
    return TelegramConfig {..}
  acVk <- section "Vk" $ do
    vcToken <- Vk.Token <$> fieldOf "token" string
    vcGroupId <- fieldOf "group_id" number
    let vcBotConfig = botConfig
    return VkConfig {..}
  acApi <- section "Run" $ fieldOf "api" readable
  acLogLevel <- section "Run" $ fieldOf "log_level" readable
  return AppConfig {..}
