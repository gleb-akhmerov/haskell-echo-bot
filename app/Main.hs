{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.IO as T
import Data.Ini.Config ( parseIniFile )
import qualified Vk.Core as Vk
import Logger ( Level(..) )
import Config

main :: IO ()
main = do
  configText <- T.readFile "config.ini"
  case parseIniFile configText configParser of
    Left err ->
      putStrLn err
    Right config ->
      Vk.runBot Debug (acVk config)
