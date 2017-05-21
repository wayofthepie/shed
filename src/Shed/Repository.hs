{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shed.Repository where

import Data.Aeson (FromJSON, ToJSON, decode, encode, toJSON)
import Data.ByteString
import Data.ByteString.Lazy as BL (fromStrict, toStrict)
import Data.Either (either)
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Redis hiding (decode)


-- | Known errors that can occur in our repository.
data Error = JsonRetrievalError T.Text
  | JsonDecodeError T.Text


-- | Keys for starage and retrieval of values.
newtype Key = Key T.Text deriving (Eq, Show)


-- | Store the given JSON value at the given key.
storeJson :: (RedisCtx m (Either Reply), ToJSON a)
          => Key -> a -> m (Either Reply ByteString)
storeJson (Key key) json = do
  let encoded = encode (toJSON json)
  sendRequest ["JSON.set", T.encodeUtf8 key, ".", BL.toStrict encoded]


-- | Get a json value from the given key.
getJson :: (RedisCtx m (Either Reply), FromJSON a)
        => Key -> m (Either Error a)
getJson k@(Key key) = pure . either jsonRetrievalError decodeJson =<< getJsonBS k
  where
    -- | Decode a json bytestring into a value.
    decodeJson :: FromJSON a => ByteString -> Either Error a
    decodeJson json = maybe jsonDecodeError Right (decode (BL.fromStrict json))

    -- | Handle a decode error.
    jsonDecodeError :: Either Error a
    jsonDecodeError = Left . JsonDecodeError $
      T.append "Could not decode json value at key " key

    -- | Handle the possible retrieval errors.
    jsonRetrievalError :: Reply -> Either Error a
    jsonRetrievalError (Error e) = Left . JsonRetrievalError $
      T.concat
        [ "Could not retrieve json value for key "
        , key
        , ".\nError: "
        , T.decodeUtf8 e
        ]
    jsonRetrievalError (Bulk Nothing) = Left . JsonRetrievalError $
      T.append "Error: Nothing to retrieve for key " key
    jsonRetrievalError _ = Left . JsonRetrievalError $
      T.append "Unknown error occurred retrieving value for key " key


-- | Get the json value at the given key, as a bytestring.
getJsonBS :: (RedisCtx m (Either Reply)) => Key -> m (Either Reply ByteString)
getJsonBS (Key key) = sendRequest ["JSON.get", T.encodeUtf8 key, "."]
