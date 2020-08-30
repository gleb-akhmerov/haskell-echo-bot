{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Logger where

import Data.Functor.Identity ( Identity, runIdentity )
import Control.Monad.Trans.Identity ( IdentityT(..) )
import Control.Monad.Reader ( ReaderT, ask, runReaderT )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad ( when )

data Level
   = Debug
   | Info
   | Warning
   | Error
  deriving (Show, Eq, Ord)

class Monad m => MonadLogger m where
  logLn :: Level -> String -> m ()

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadLogger m
  ) => MonadLogger (t m) where
  logLn level msg = lift (logLn level msg)


newtype ConsoleLoggerT m a
  = ConsoleLoggerT { unConsoleLoggerT :: ReaderT Level m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance MonadIO m => MonadLogger (ConsoleLoggerT m) where
  logLn level message = ConsoleLoggerT $ do
    minLevel <- ask
    liftIO $ when (level >= minLevel) $
      putStrLn $ "[" ++ show level ++ "] " ++ message

runConsoleLoggerT :: MonadIO m => ConsoleLoggerT m a -> Level -> m a
runConsoleLoggerT (ConsoleLoggerT m) minLevel =
  runReaderT m minLevel


newtype NoLoggingT m a
  = NoLoggingT { runNoLoggingT :: m a }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadTrans via IdentityT

instance Monad m => MonadLogger (NoLoggingT m) where
  logLn _ _ = return ()

runNoLogging :: NoLoggingT Identity a -> a
runNoLogging m = runIdentity $ runNoLoggingT m
