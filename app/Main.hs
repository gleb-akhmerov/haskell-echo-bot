module Main where

import Bot (react, Message(..))

main :: IO ()
main = print $ react $ Message { text = "Echo me" }
