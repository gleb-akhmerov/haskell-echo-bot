{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Logger where

import Prelude hiding ( log )
import Data.Functor.Identity ( Identity, runIdentity )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.Writer ( WriterT, Writer, tell )
import Control.Monad.State ( StateT )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad ( when )

data Level
   = Debug
   | Info
   | Warning
   | Error
  deriving (Show, Eq, Ord)

-- This is essentially (ReaderT Level) but with a different ask function,
-- so that it is possible to use ReaderT and Logger at the same time.

newtype LevelReader m a = LevelReader { runLevelReader :: Level -> m a }

instance Monad m => Functor (LevelReader m) where
  fmap f m = LevelReader $ (fmap f) . runLevelReader m

instance Monad m => Applicative (LevelReader m) where
  pure x = LevelReader $ const $ pure x
  (LevelReader f) <*> (LevelReader x) = LevelReader $ \l -> f l <*> x l

instance Monad m => Monad (LevelReader m) where
  return = pure
  x >>= f = LevelReader $ \l -> do
      a <- runLevelReader x l
      runLevelReader (f a) l

instance MonadTrans LevelReader where
  lift m = LevelReader (const m)

instance MonadIO m => MonadIO (LevelReader m) where
  liftIO = lift . liftIO

class Monad m => MonadLevelReader m where
  getLevel :: m Level

instance Monad m => MonadLevelReader (LevelReader m) where
  getLevel = LevelReader return

class MonadLevelReader m => MonadLogger m where
  doLog :: String -> m ()

  log :: Level -> String -> m ()
  log level message = do
    minLevel <- getLevel
    when (level >= minLevel) $
      doLog $ "[" ++ show level ++ "] " ++ message  

instance MonadLevelReader m => MonadLevelReader (ReaderT r m) where
  getLevel = lift getLevel

instance MonadLevelReader m => MonadLevelReader (StateT r m) where
  getLevel = lift getLevel

instance (Monoid w, MonadLevelReader m) => MonadLevelReader (WriterT w m) where
  getLevel = lift getLevel

instance MonadLogger m => MonadLogger (ReaderT r m) where
  doLog = lift . doLog

instance MonadLogger m => MonadLogger (StateT s m) where
  doLog = lift . doLog

instance (Monoid w, MonadLogger m) => MonadLogger (WriterT w m) where
  doLog = lift . doLog

instance MonadLogger (LevelReader IO) where
  doLog = liftIO . putStrLn

instance MonadLogger (LevelReader (Writer [String])) where
  doLog mes = lift $ tell [mes]

instance MonadLogger (LevelReader Identity) where
  doLog _ = return ()

runLogger :: Monad m => Level -> LevelReader m a -> m a
runLogger minLevel f = runLevelReader f minLevel

runLoggerIgnore :: LevelReader Identity a -> a
runLoggerIgnore f = runIdentity $ runLogger Debug f
