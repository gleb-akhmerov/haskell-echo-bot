module Main where

import Control.Monad.State.Lazy (evalState)
import Bot (react, InMessage(..), defaultConfig)

main :: IO ()
main = print $ evalState (react $ InTextMessage "Echo me") defaultConfig
