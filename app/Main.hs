module Main where

import Bot (react, InMessage(..), defaultConfig)

main :: IO ()
main = print $ react defaultConfig (InTextMessage "Echo me")
