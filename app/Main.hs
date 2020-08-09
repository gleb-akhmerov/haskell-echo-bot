{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State ( evalStateT )
import qualified Data.Text.IO as T
import Data.Ini.Config ( IniParser, section, fieldOf, string, parseIniFile )
import qualified Telegram
import Bot ( defaultConfig )

data AuthConfig = AuthConfig
  { telegramToken :: String }
  deriving Show

configParser :: IniParser AuthConfig
configParser = do
  section "Telegram" $ do
    tgToken <- fieldOf "token" string
    return AuthConfig { telegramToken = tgToken }

main :: IO ()
main = do
  configText <- T.readFile "config.ini"
  case parseIniFile configText configParser of
    Left err -> putStrLn err
    Right config -> evalStateT (Telegram.runBot (telegramToken config) 0 defaultConfig) 1
