module Main where

import Bot (react, Message(..), defaultConfig)

main :: IO ()
main = print $ react defaultConfig (Message "Echo me")
