{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Util where

import Data.Function ( (&) )
import Network.HTTP.Simple ( setRequestBodyJSON, parseRequest_, setRequestMethod, Request, setRequestQueryString )
import Data.Aeson ( eitherDecode, FromJSON )
import Data.Aeson.Types ( Value )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either.Combinators ( mapLeft )

requestJSON :: String -> Value -> Request
requestJSON url json =
  parseRequest_ url
    & setRequestMethod "POST"
    & setRequestBodyJSON json

requestQuery :: String -> [(String, String)] -> Request
requestQuery url query =
  parseRequest_ url
    & setRequestQueryString (map (\(k, v) -> (BS.pack k, Just $ BS.pack v)) query)

verboseEitherDecode :: FromJSON a => LBS.ByteString -> Either String a
verboseEitherDecode jsonStr =
  mapLeft
    (\err -> err ++ " " ++ LBS.unpack jsonStr)
    (eitherDecode jsonStr)
