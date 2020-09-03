{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.IO as T
import Data.Ini.Config ( parseIniFile )
import qualified Telegram as Tg
import qualified Vk
import Config

main :: IO ()
main = do
  configText <- T.readFile "config.ini"
  case parseIniFile configText configParser of
    Left err ->
      putStrLn err
    Right config ->
      case (cApi config) of
        Telegram ->
          Tg.runBot (cLogLevel config) (cBot config) (cTelegramToken config)
        VK ->
          Vk.runBot (cLogLevel config) (cBot config) (cVk config)
