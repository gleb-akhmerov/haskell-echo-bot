{-# LANGUAGE NamedFieldPuns #-}

module Bot
    ( Config(..)
    , InMessage(..)
    , OutMessage(..)
    , react
    , defaultConfig
    ) where


data Config = Config { repeats :: Int
                     , helpText :: String
                     , repeatKeyboardText :: String
                     } deriving (Show, Eq)

defaultConfig =
  Config
    { repeats = 1
    , helpText = "This bot repeats the messages that you send it."
    , repeatKeyboardText = "How many times would you want me to repeat my messages?"
    }

data InMessage
  = InTextMessage String
  | KeyboardKeyPushed Int
  deriving (Show, Eq)

data OutMessage
  = SendMessageTimes Int String
  | SendKeyboard String [Int]
  deriving (Show, Eq)

react :: Config -> InMessage -> (Config, OutMessage)
react config message = case message of
  InTextMessage text ->
    case text of
      "/help" ->
        (config, SendMessageTimes 1 (helpText config))
      "/repeat" ->
        (config, SendKeyboard (repeatKeyboardText config) [1, 2, 3, 4, 5])
      otherwise ->
        (config, SendMessageTimes (repeats config) text)
  KeyboardKeyPushed n ->
    (config { repeats = n }, SendMessageTimes 1 ("The messages will now be repeated " ++ show n ++ " times."))
