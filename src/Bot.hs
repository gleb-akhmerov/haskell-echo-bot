{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot where

import Control.Monad.State ( get, put, MonadState )

data Config = Config { helpText :: String
                     , repeatKeyboardText :: String
                     } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig =
  Config
    { helpText = "This bot repeats the messages that you send it."
    , repeatKeyboardText = "How many times would you like me to repeat your messages?"
    }

data InMessage a
  = InTextMessage String a
  | InMediaMessage a
  | KeyboardKeyPushed Int
  deriving (Show, Eq)

data OutMessage a
  = EchoTimes Int a
  | SendText String
  | SendKeyboard String [Int]
  deriving (Show, Eq)

react :: MonadState Int m => Config -> InMessage a -> m (OutMessage a)
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
