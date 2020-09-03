{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import Data.Ini.Config
import qualified Telegram.Api as Tg ( Token(..) )
import qualified Vk.Api as Vk ( Token(..) )
import qualified Vk ( Config(..) )
import qualified Bot ( Config(..) )
import Logger ( Level )

data Api
  = Telegram
  | VK
  deriving (Read, Show, Eq)

data Config
   = Config
       { cTelegramToken :: Tg.Token
       , cVk :: Vk.Config
       , cBot :: Bot.Config
       , cApi :: Api
       , cLogLevel :: Level
       }
  deriving (Show, Eq)

configParser :: IniParser Config
configParser = do
  cBot <- section "Bot" $ do
    helpText <- fieldOf "help_text" string
    repeatKeyboardText <- fieldOf "repeat_keyboard_text" string
    initialRepeats <- fieldOf "initial_repeats" number
    return Bot.Config {..}
  cTelegramToken <- fmap Tg.Token $ section "Telegram" $ fieldOf "token" string
  cVk <- section "Vk" $ do
    cToken <- Vk.Token <$> fieldOf "token" string
    cGroupId <- fieldOf "group_id" number
    return Vk.Config {..}
  cApi <- section "Run" $ fieldOf "api" readable
  cLogLevel <- section "Run" $ fieldOf "log_level" readable
  return Config {..}
