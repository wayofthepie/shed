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
import qualified Shed.Step.Model as Model
import qualified Shed.Step.Repository as Repo
import Shed.Redis -- Should not import this, need to sort error types...

-- | Create a Step.
postStepH :: Key -> Model.Step -> AppT IO ()
postStepH key step = do
  pool <- ask
  eitherError <- liftIO $ runRedis pool (Repo.storeStep key step)
  either decodeError pure eitherError
  where
    decodeError (JsonStoreError e) = customError err500 (T.unpack e)

-- | Get a Step.
getStepH :: Key -> AppT IO Model.StepsModule
getStepH key = do
  pool <- ask
  e <- liftIO $ runRedis pool (Repo.getStepsModule key)
  either decodeError pure e
  where
    decodeError (JsonRetrievalError e) = customError err500 (T.unpack e)
    decodeError (JsonDecodeError e) = customError err404 (T.unpack e)
