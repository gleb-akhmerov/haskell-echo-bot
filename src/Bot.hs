{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot where

import Prelude hiding ( log )
import Control.Monad.State ( get, put, MonadState )
import Logger

data Config
   = Config
       { helpText :: String
       , repeatKeyboardText :: String
       , initialRepeats :: Int
       }
  deriving (Show, Eq)

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

react :: (MonadState Int m, Show a, MonadLogger m) => Config -> InMessage a -> m (OutMessage a)
react config inMessage = do
  log Debug (show inMessage)
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
