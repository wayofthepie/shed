{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies   #-}

module Shed.Step.Model where

import Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

-- | Database schema and persistable entities for steps.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Step
  name T.Text
  version Int
  UniqueName name
  deriving Eq Read Show
Executable
  cmd T.Text
  args [T.Text]
  step StepId
  deriving Eq Read Show
|]
