{-# LANGUAGE NamedFieldPuns #-}

module Bot
    ( react
    , Message(..)
    ) where


data Message = Message { text :: String } deriving Show

react :: Message -> [Message]
react (Message { text } ) = [Message { text = text }]
