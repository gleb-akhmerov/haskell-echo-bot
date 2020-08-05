{-# LANGUAGE NamedFieldPuns #-}

module Bot
    ( react
    , Config(..)
    , Message(..)
    ) where


data Config = Config { repeats :: Int }

data Message = Message { text :: String } deriving Show

react :: Config -> Message -> [Message]
react (Config { repeats }) (Message { text })
  = replicate repeats $ Message { text = text }
