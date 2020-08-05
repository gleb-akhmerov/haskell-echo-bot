{-# LANGUAGE NamedFieldPuns #-}

module Bot
    ( Action(..)
    , Config(..)
    , InMessage(..)
    , OutMessage(..)
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

data InMessage = InTextMessage String deriving (Show, Eq)

data OutMessage = OutTextMessage String deriving (Show, Eq)

data Action
  = Echo String
  | Help
  deriving (Show, Eq)

messageToAction :: InMessage -> Action
messageToAction (InTextMessage text) = case text of
  "/help"   -> Help
  otherwise -> Echo text

react :: Config -> InMessage -> [OutMessage]
react config message
  = case messageToAction message of
      Echo text -> replicate (repeats config) $ OutTextMessage text
      Help -> [OutTextMessage $ helpText config]
