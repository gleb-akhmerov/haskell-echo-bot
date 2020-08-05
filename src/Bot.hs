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

data Action
  = Echo String
  | Help
  | RequestRepeatKeyboard
  | SetRepeatTo Int
  deriving (Show, Eq)

messageToAction :: InMessage -> Action
messageToAction message = case message of
  InTextMessage text ->
    case text of
      "/help"   -> Help
      "/repeat" -> RequestRepeatKeyboard
      otherwise -> Echo text
  KeyboardKeyPushed n -> SetRepeatTo n

react :: Config -> InMessage -> (Config, [OutMessage])
react config message
  = case messageToAction message of
      Echo text ->
        (config, replicate (repeats config) $ OutTextMessage text)
      Help ->
        (config, [OutTextMessage $ helpText config])
      RequestRepeatKeyboard ->
        (config, [SendKeyboard (repeatKeyboardText config) [1, 2, 3, 4, 5]])
      SetRepeatTo n ->
        (config { repeats = n }, [OutTextMessage $ "The messages will now be repeated " ++ show n ++ " times."])
