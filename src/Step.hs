{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Step where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson.TH
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Model

data Step = Step
  { name :: T.Text
  , execute :: [Executable]
  } deriving (Eq, Show)

data Executable = Executable
  { cmd :: T.Text
  , args :: [T.Text]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Step)
$(deriveJSON defaultOptions ''Executable)


-- | Persist a step.
createStep :: ConnectionPool -> Step -> IO (Either T.Text (Key Model.Step))
createStep pool step = flip runSqlPersistMPool pool $ do
  exists <- selectFirst [Model.StepName ==. name step] []
  case exists of
    Nothing -> do
      let stepModel = stepToModel step
      createdStepKey <- insert stepModel
      (Just createdStep) <- get createdStepKey
      createExecs (execute step) createdStepKey
      pure . Right $ createdStepKey
    Just _ -> pure . Left $ T.pack "Step already exists!"
  where
    -- createExecs :: [Executable] -> IO [Key Model.Executable]
    createExecs execs step = do
      let modelExecs = fmap (executableToModel step) execs
      insertMany_ modelExecs

getStepFromName :: ConnectionPool -> T.Text -> IO (Either T.Text Step)
getStepFromName pool stepName = flip runSqlPersistMPool pool $ do
  maybeStepName <- getBy (Model.UniqueName stepName)
  case maybeStepName of
    Nothing -> pure . Left $ T.pack "Does not exist"
    Just entity -> constructStepFromEntity entity >>= pure . Right
  where
    constructStepFromEntity (Entity stepId stepModel) = do
      execModelEntities <- selectList [Model.ExecutableStep ==. stepId] []
      let execs = fmap entityToExecutable execModelEntities
      pure $ modelToStep stepModel execs

-- | Build an Executable model from the given step Id and Executable.
executableToModel :: (Key Model.Step) -> Executable -> Model.Executable
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
entityToExecutable :: (Entity Model.Executable) -> Executable
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
