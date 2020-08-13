{-# LANGUAGE NamedFieldPuns #-}

module Bot where

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

data InMessage echoable
  = InTextMessage String echoable
  | InMediaMessage echoable
  | KeyboardKeyPushed Int
  deriving (Show, Eq)

data OutMessage echoable
  = EchoTimes Int echoable
  | SendText String
  | SendKeyboard String [Int]
  deriving (Show, Eq)

react :: Monad m => Config -> InMessage echoable -> StateT Int m (OutMessage echoable)
react config inMessage =
  case inMessage of
    InMediaMessage message -> do
      repeats <- get
      return $ EchoTimes repeats message
    InTextMessage text message ->
      case text of
        "/help" ->
          return $ SendText $ helpText config
        "/repeat" ->
          return $ SendKeyboard (repeatKeyboardText config) [1, 2, 3, 4, 5]
        _ -> do
          repeats <- get
          return $ EchoTimes repeats message
    KeyboardKeyPushed n -> do
      put n
      return $ SendText $ "The messages will now be repeated " ++ show n ++ " times."
