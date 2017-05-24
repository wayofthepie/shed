{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Shed.Step.Handler where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Redis
import Servant

import Shed.Error
import Shed.Types
import qualified Shed.Step.Persist.Model as Model
import qualified Shed.Step.Persist.Repository as Repo
import Shed.Redis -- Should not import this, need to sort error types...

-- | Create a Step.
postModuleVersionH :: T.Text -> Model.ModuleVersion -> AppT IO ()
postModuleVersionH key step = do
  pool <- ask
  eitherError <- liftIO $ runRedis pool (Repo.setMasterForModule (Model.ModuleNs key) step)
  either decodeError pure eitherError
  where
    decodeError _ = customError err500 (T.unpack "Error!")

-- | Get a Step.
getModuleVersionH :: T.Text -> AppT IO Model.ModuleVersion
getModuleVersionH key = do
  pool <- ask
  e <- liftIO $ runRedis pool (Repo.getMasterForModule $ Model.ModuleNs key)
  either decodeError pure e
  where
    decodeError (JsonRetrievalError e) = customError err500 (T.unpack e)
    decodeError (JsonDecodeError e) = customError err404 (T.unpack e)
