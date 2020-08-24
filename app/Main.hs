{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.IO as T
import Data.Ini.Config ( IniParser, section, fieldOf, string, parseIniFile, number )
import qualified Telegram.Api as Tg
import qualified Vk.Core as Vk
import qualified Vk.Api as Vk
import Bot

data AuthConfig
   = AuthConfig
       { telegramToken :: Tg.Token
       , vkToken :: Vk.Token
       , vkGroupId :: Integer
       }
  deriving Show

configParser :: IniParser AuthConfig
configParser = do
  telegramToken <- Tg.Token <$> (section "Telegram" $ fieldOf "token" string)
  vkToken <- Vk.Token <$> (section "VK" $ fieldOf "token" string)
  vkGroupId <- section "VK" $ fieldOf "groupId" number
  return AuthConfig {..}

main :: IO ()
main = do
  configText <- T.readFile "config.ini"
  case parseIniFile configText configParser of
    Left err ->
      putStrLn err
    Right config ->
      Vk.runBot defaultConfig 1 (vkToken config) (vkGroupId config)
