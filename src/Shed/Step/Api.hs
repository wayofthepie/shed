{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Shed.Step.Api where

import qualified Data.Text as T
import Servant

import Shed.Redis (Key)
import Shed.Step.Persist.Model

type StepModuleApi =
  -- /module/{moduleName}
  "module"
    :> Capture "moduleName" T.Text
    :> ReqBody '[JSON] ModuleVersion
    :> PostNoContent '[JSON] ()
  :<|> "module"
    :> Capture "moduleName" T.Text
    :> Get '[JSON] ModuleVersion
