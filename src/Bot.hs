{-# LANGUAGE NamedFieldPuns #-}

module Bot
    ( Action(..)
    , Config(..)
    , Message(..)
    , react
    , messageToAction
    , defaultConfig
    ) where


data Config = Config { repeats :: Int, helpText :: String }

defaultConfig =
  Config
    { repeats = 1
    , helpText = "This bot repeats the messages that you send it."
    }

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
react config message
  = case messageToAction message of
      Echo text -> replicate (repeats config) $ Message text
      Help -> [Message $ helpText config]
