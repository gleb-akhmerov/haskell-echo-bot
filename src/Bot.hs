{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot where

import Control.Monad.State
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

class Monad m => MonadBot i m where
  react :: Config -> InMessage i -> m (OutMessage i)

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadBot i m
  ) => MonadBot i (t m) where
  react config inMessage = lift (react config inMessage)

newtype BotT m a
  = BotT { unBotT :: StateT Int m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (MonadLogger m, Show i) => MonadBot i (BotT m) where
  react config inMessage = BotT $ do
    logLn Debug (show inMessage)
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

runBotT :: Monad m => BotT m a -> Int -> m (a, Int)
runBotT (BotT m) initialRepeats = runStateT m initialRepeats

evalBotT :: Monad m => BotT m a -> Int -> m a
evalBotT (BotT m) initialRepeats = evalStateT m initialRepeats
