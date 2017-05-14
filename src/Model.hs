{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies   #-}

module Model where

import Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH


-- | Database schema and persistable entities for shed.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Step
  name T.Text
  UniqueName name
  Primary name
  deriving Eq Read Show
Executable
  cmd T.Text
  args [T.Text]
  step StepId
  deriving Eq Read Show
|]
