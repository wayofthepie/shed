{-# LANGUAGE TypeFamilies #-}
module Shed.Step (
  module Shed.Step.Api
  , Model.migrateAll
  , createStep
  , getStepFromName
  ) where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson.TH
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

import Shed.Step.Api
import qualified Shed.Step.Model as Model

--------------------------------------------------------------------------------
-- CRUD.
--------------------------------------------------------------------------------
-- | Persist a Step.
createStep :: ConnectionPool -> Step -> IO (Either T.Text T.Text)
createStep pool step = flip runSqlPersistMPool pool $ do
  exists <- selectFirst [Model.StepName ==. name step] []
  case exists of
    Nothing -> do
      let stepModel = stepToModel step
      createdStepKey <- insert stepModel
      createExecs (execute step) createdStepKey
      maybeStep <- get createdStepKey
      pure $ case maybeStep of
        Nothing ->
          packLeft "Something went wrong retrieving the created Step key..."
        Just (Model.Step name) ->
          Right name
    Just _ -> pure alreadyExistsError
  where
    createExecs execs stepKey = do
      let modelExecs = fmap (executableToModel stepKey) execs
      insertMany_ modelExecs

    alreadyExistsError =
      packLeft ("Step " ++ T.unpack (name step) ++ " already exists!")

-- | Retrieve the Step corresponding to the given name.
getStepFromName :: ConnectionPool -> T.Text -> IO (Either T.Text Step)
getStepFromName pool stepName = flip runSqlPersistMPool pool $ do
  maybeStepName <- getBy (Model.UniqueName stepName)
  case maybeStepName of
    Nothing -> pure  doesNotExistError
    Just entity -> constructStepFromEntity entity
  where
    constructStepFromEntity (Entity stepId stepModel) = do
      execModelEntities <- selectList [Model.ExecutableStep ==. stepId] []
      let execs = fmap entityToExecutable execModelEntities
      pure . Right $ modelToStep stepModel execs

    doesNotExistError =
      packLeft ("Step " ++ T.unpack stepName ++ " does not exist!")

--------------------------------------------------------------------------------
-- Transformation.
--------------------------------------------------------------------------------
-- | Build an Executable model from the given step Id and Executable.
executableToModel :: Key Model.Step -> Executable -> Model.Executable
executableToModel step exec= Model.Executable
  { Model.executableCmd = cmd exec
  , Model.executableArgs = args exec
  , Model.executableStep = step
  }

-- | Build a Step model from the given step.
stepToModel :: Step -> Model.Step
stepToModel step = Model.Step
  { Model.stepName = name step
  }

-- | Build an Executable from an Executable model Entity.
entityToExecutable :: Entity Model.Executable -> Executable
entityToExecutable (Entity modelId model) = Executable
  { cmd = Model.executableCmd model
  , args = Model.executableArgs model
  }

-- | Build a Step from a Step model and a list of Executables.
modelToStep :: Model.Step -> [Executable] -> Step
modelToStep model execs = Step
  { name = Model.stepName model
  , execute = execs
  }

-- | Pack a String as a Left Text.
packLeft :: String -> Either T.Text a
packLeft = Left . T.pack
