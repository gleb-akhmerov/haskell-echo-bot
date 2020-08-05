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
  = OutTextMessage String
  | SendKeyboard String [Int]
  deriving (Show, Eq)

react :: Config -> InMessage -> (Config, [OutMessage])
react config message = case message of
  InTextMessage text ->
    case text of
      "/help" ->
        (config, [OutTextMessage $ helpText config])
      "/repeat" ->
        (config, [SendKeyboard (repeatKeyboardText config) [1, 2, 3, 4, 5]])
      otherwise ->
        (config, replicate (repeats config) $ OutTextMessage text)
  KeyboardKeyPushed n ->
    (config { repeats = n }, [OutTextMessage $ "The messages will now be repeated " ++ show n ++ " times."])
