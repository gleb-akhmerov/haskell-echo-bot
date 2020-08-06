module Main where

import Control.Monad.State.Lazy (evalState)
import Bot (react, InMessage(..), defaultConfig)

main :: IO ()
main = print $ evalState (react defaultConfig (InTextMessage "Echo me")) 1
