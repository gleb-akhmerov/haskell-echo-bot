{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State (StateT, evalStateT, liftIO)
import qualified Data.Text.IO as T
import Data.Ini.Config (IniParser, section, fieldOf, string, parseIniFile)
import Bot (react, InMessage(..), OutMessage(..), defaultConfig)

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
  print $ parseIniFile configText configParser
