{-# LANGUAGE OverloadedStrings #-}

module Shed.Repository where

import qualified Data.ByteString as B
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Redis

storeTextAt :: T.Text -> T.Text -> Redis (Either T.Text T.Text)
storeTextAt k t = do
  set (T.encodeUtf8 k) (T.encodeUtf8 t)
  eitherVal <- get (T.encodeUtf8 k)
  pure $ either
    isError
    (maybe (Left "No key found for....") (\b -> Right $ T.decodeUtf8 b))
    eitherVal

isError :: Reply -> Either T.Text T.Text
isError r = case r of
   Error b -> Left (T.decodeUtf8 b)
   _       -> Right (T.decodeUtf8 "")
