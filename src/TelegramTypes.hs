{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module TelegramTypes where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON(..), camelTo2, fieldLabelModifier, genericParseJSON, defaultOptions )

data Result a
   = Result
       { rOk :: Bool
       , rDescription :: Maybe String
       , rResult :: Maybe a
       }
   deriving ( Generic, Show, Eq )

data Update
   = Update
       { uUpdateId :: Integer
       , uMessage :: Maybe Message
       , uCallbackQuery :: Maybe CallbackQuery
       }
   deriving ( Generic, Show, Eq )

data CallbackQuery
   = CallbackQuery
       { cqId :: String
       , cqFrom :: User
       , cqData :: String
       }
   deriving ( Generic, Show, Eq )

data Message
   = Message
       { mFrom :: User
       , mText :: Maybe String
       , mAnimation :: Maybe Animation
       , mAudio :: Maybe Audio
       , mDocument :: Maybe Document
       , mSticker :: Maybe Sticker
       }
   deriving ( Generic, Show, Eq )

data User
   = User { uId :: Integer }
   deriving ( Generic, Show, Eq )

data Animation
   = Animation { animationFileId :: String }
   deriving ( Generic, Show, Eq )

data Audio
   = Audio { audioFileId :: String }
   deriving ( Generic, Show, Eq )

data Document
   = Document { documentFileId :: String }
   deriving ( Generic, Show, Eq )

data Sticker
   = Sticker { stickerFileId :: String }
   deriving ( Generic, Show, Eq )

instance FromJSON a => FromJSON (Result a) where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON CallbackQuery where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 2 }

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

instance FromJSON Animation where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop (length "Animation") }

instance FromJSON Audio where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop (length "Audio") }

instance FromJSON Document where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop (length "Document") }

instance FromJSON Sticker where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop (length "Sticker") }
