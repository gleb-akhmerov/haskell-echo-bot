module Main where

import Control.Monad.State (StateT, evalStateT, liftIO)
import Bot (react, InMessage(..), OutMessage(..), defaultConfig)

loop :: StateT Int IO ()
loop = do
  inText <- liftIO getLine
  SendMessageTimes n outText <- react defaultConfig (InTextMessage inText)
  liftIO $ mapM_ putStrLn (replicate n outText)
  loop

main :: IO ()
main = evalStateT loop 2
