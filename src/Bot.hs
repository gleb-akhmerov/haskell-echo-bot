{-# LANGUAGE NamedFieldPuns #-}

module Bot
    ( Action(..)
    , Config(..)
    , Message(..)
    , react
    , messageToAction
    ) where


data Config = Config { repeats :: Int }

data Message = Message String deriving (Show, Eq)

data Action
  = Echo String
  | Help
  deriving (Show, Eq)

messageToAction :: Message -> Action
messageToAction (Message text) = case text of
  "/help"   -> Help
  otherwise -> Echo text

react :: Config -> Message -> [Message]
react (Config { repeats }) (Message text)
  = replicate repeats $ Message text
