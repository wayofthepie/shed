{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shed.Redis where

import Control.Monad
import Control.Monad.Operational
import Data.Aeson (FromJSON, ToJSON, decode, encode, toJSON)
import Data.ByteString hiding (singleton)
import Data.ByteString.Lazy as BL (fromStrict, toStrict)
import Data.Either (either)
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Redis hiding (decode)

import Shed.Types

-- | Known errors that can occur in our repository.
data RedisError =
  JsonError T.Text -- ^ Wraps errors from Redis directly.
  | JsonRetrievalError T.Text -- ^ Occurs when a key has no value.
  | JsonDecodeError T.Text -- ^ Occurs when decoding fails.
  | JsonUnknownError T.Text -- ^ For unexpected conditions.
  | TransactionAborted T.Text -- ^ Occurs when a transaction was aborted.
  deriving (Eq, Show)

-- | Keys for storage and retrieval of values.
newtype Key = Key T.Text deriving (Eq, Show)

-- | A json path used when retrieving or updating nested values.
newtype JsonPath = JsonPath T.Text deriving (Eq, Show)


jsonSet :: (RedisCtx m f, ToJSON a) => Key -> JsonPath -> a -> m (f ByteString)
jsonSet (Key key) (JsonPath path) val =
  let encodedJson = BL.toStrict . encode . toJSON $ val
  in  sendRequest ["JSON.SET", T.encodeUtf8 key, T.encodeUtf8 path, encodedJson]


jsonGet :: (RedisCtx m f) => Key -> JsonPath -> m (f ByteString)
jsonGet (Key key) (JsonPath path) = sendRequest ["JSON.GET", T.encodeUtf8 key, T.encodeUtf8 path]


jsonObjLen :: RedisCtx m f => Key -> JsonPath -> m (f Integer)
jsonObjLen (Key key) (JsonPath path) = sendRequest ["JSON.OBJLEN", T.encodeUtf8 key, T.encodeUtf8 path]


-- | Decode a 'TxResult'.
decodeTxResult :: TxResult a -> Either RedisError a
decodeTxResult (TxSuccess val) = Right val
decodeTxResult TxAborted = Left (TransactionAborted "Transaction was aborted!")
decodeTxResult (TxError err) = Left (JsonError (T.pack err))


-- | Decode a redis 'Reply'. This assumes a 'Reply' is always an error of some kind.
decodeReply :: Reply -> RedisError
decodeReply (Error bs) = JsonError (T.decodeUtf8 bs)
decodeReply (Bulk maybeBs) = maybe
  (JsonRetrievalError "No json value found for the given key.")
  (\_ -> JsonDecodeError "Data did not decode correctly, received a Bulk reply with data!") -- ???
  maybeBs
decodeReply _ = JsonUnknownError "Something unexpected happened ¯\\_(ツ)_/¯"
