{-# LANGUAGE NamedFieldPuns #-}

module Bot
    ( Config(..)
    , InMessage(..)
    , OutMessage(..)
    , react
    , defaultConfig
    ) where

import Control.Monad.State (StateT, get, put)

data Config = Config { helpText :: String
                     , repeatKeyboardText :: String
                     } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig =
  Config
    { helpText = "This bot repeats the messages that you send it."
    , repeatKeyboardText = "How many times would you like me to repeat your messages?"
    }

data InMessage
  = InTextMessage String
  | KeyboardKeyPushed Int
  deriving (Show, Eq)

data OutMessage
  = SendMessageTimes Int String
  | SendKeyboard String [Int]
  deriving (Show, Eq)

react :: Monad m => Config -> InMessage -> StateT Int m OutMessage
react config message =
  case message of
    InTextMessage text ->
      case text of
        "/help" ->
          return $ SendMessageTimes 1 (helpText config)
        "/repeat" ->
          return $ SendKeyboard (repeatKeyboardText config) [1, 2, 3, 4, 5]
        _ -> do
          repeats <- get
          return $ SendMessageTimes repeats text
    KeyboardKeyPushed n -> do
      put n
      return $ SendMessageTimes 1 ("The messages will now be repeated " ++ show n ++ " times.")
