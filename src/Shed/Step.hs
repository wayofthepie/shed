{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Shed.Step (
  module Shed.Step.Api
  , Model.migrateAll
  , createStep
  , getStepFromName
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy as TL
import Database.Persist
import Database.Persist.Sql
import Servant

import Shed.Step.Api
import qualified Shed.Step.Model as Model
import Shed.Types (AppT(..))

--------------------------------------------------------------------------------
-- CRUD.
--------------------------------------------------------------------------------
-- | Persist a Step.
createStep :: Step -> AppT IO StepCreationSuccess
createStep step = do
    exists <- runQuery $ selectFirst [Model.StepName ==. name step] []
    case exists of
      Nothing -> do
        createdStepKey <- runQuery $ insert (stepToModel step)
        createExecs (executables step) createdStepKey
        maybeStep <- runQuery $ get createdStepKey
        case maybeStep of
          Nothing -> unknownError
          Just (Model.Step n) -> pure (StepCreationSuccess n)
      Just _ -> alreadyExistsError
  where
    createExecs execs stepKey = do
      let modelExecs = fmap (executableToModel stepKey) execs
      runQuery $ insertMany_ modelExecs

    alreadyExistsError = customError
      err409
      ("Step " ++ T.unpack (name step) ++ " already exists!")

    unknownError = customError
      err500
      "Something went wrong retrieving the created Step key..."

-- | Retrieve the Step corresponding to the given name.
getStepFromName :: T.Text -> AppT IO Step
getStepFromName stepName = do
  maybeStepName <- runQuery $ getBy (Model.UniqueName stepName)
  case maybeStepName of
    Nothing -> doesNotExistError
    Just entity -> constructStepFromEntity entity
  where
    constructStepFromEntity (Entity stepEntityId stepModel) = do
      execModelEntities <- runQuery $
        selectList [Model.ExecutableStep ==. stepEntityId] []
      let execs = fmap entityToExecutable execModelEntities
      pure (modelToStep stepModel execs)

    doesNotExistError = customError
      err404
      ("Step " ++ T.unpack stepName ++ " does not exist!")

-- | Throw a custom ServantErr.
customError :: MonadError ServantErr m => ServantErr -> String -> m a
customError errF msg = throwError (errF
    { errBody = TE.encodeUtf8 . TL.fromStrict . T.pack $ msg
    })

-- | Run the query on a connection from the pool.
runDb :: ConnectionPool -> SqlPersistT (AppT IO) a -> AppT IO a
runDb pool q = runSqlPool q pool

-- | Run the query.
runQuery :: SqlPersistT (AppT IO) a -> AppT IO a
runQuery query = do
  pool <- ask
  runDb pool query

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
entityToExecutable (Entity _ model) = Executable
  { cmd = Model.executableCmd model
  , args = Model.executableArgs model
  }

-- | Build a Step from a Step model and a list of Executables.
modelToStep :: Model.Step -> [Executable] -> Step
modelToStep model execs = Step
  { name = Model.stepName model
  , executables = execs
  }
