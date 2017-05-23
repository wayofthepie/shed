{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shed.Step.Persist.Repository where

import Data.Aeson (ToJSON, decode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Redis hiding (decode)

import Shed.Redis
import Shed.Step.Persist.Model

-- | This repository deals with modules. Modules are versioned collections of steps.
-- module:{moduleName} - Json object relating to the master version of this module.
-- module:{moduleName}:versions - Sorted set of versions.
-- module:{moduleName}:{version} - Json object relating to this version of the module.

-- | Set the master version to the 'ModuleVersion' json value, for the given 'ModuleNs' namespace.
setMasterForModule :: RedisCtx m (Either Reply) => ModuleNs -> ModuleVersion -> m (Either RedisError ())
setMasterForModule (ModuleNs ns) m =
  pure . either (Left . decodeReply) (\_ -> Right ()) =<< jsonSet (Key ns) (JsonPath ".") m


getMasterForModule :: RedisCtx m (Either Reply) => ModuleNs -> m (Either RedisError ModuleVersion)
getMasterForModule (ModuleNs ns) = pure . either (Left . decodeReply) decodeJson =<< jsonGet (Key ns) (JsonPath ".")
  where
    decodeJson :: ByteString -> Either RedisError ModuleVersion
    decodeJson bs = maybe (Left . JsonDecodeError $ "Could not decode json value") Right (decode $ fromStrict bs)
