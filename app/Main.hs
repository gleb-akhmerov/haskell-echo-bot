module Main where

import Bot (react, Message(..), Config(..))

main :: IO ()
main = print $ react (Config { repeats = 5 }) (Message { text = "Echo me" })
