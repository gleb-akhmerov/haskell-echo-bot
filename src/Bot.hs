{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot where

import Control.Monad.State
import Control.Monad.Reader
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
  react :: InMessage i -> m (OutMessage i)

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadBot i m
  ) => MonadBot i (t m) where
  react inMessage = lift (react inMessage)

newtype BotT m a
  = BotT { unBotT :: ReaderT Config (StateT Int m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans BotT where
  lift = BotT . lift . lift

instance (MonadLogger m, Show i) => MonadBot i (BotT m) where
  react inMessage = BotT $ do
    config <- ask
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

runBotT :: Monad m => BotT m a -> Config -> m (a, Int)
runBotT (BotT m) config = runStateT (runReaderT m config) (initialRepeats config)

evalBotT :: Monad m => BotT m a -> Config -> m a
evalBotT (BotT m) config = evalStateT (runReaderT m config) (initialRepeats config)
