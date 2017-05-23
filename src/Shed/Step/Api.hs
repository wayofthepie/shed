{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Shed.Step.Api where

import qualified Data.Text as T
import Servant

import Shed.Step.Model

data StepsModuleVersion = StepsModuleVersion
  { version :: T.Text
  , steps :: [Step]
  }

type StepModuleApi =
  -- /module/
  "module" :> Get '[JSON] StepsModule
  -- /module/{moduleName}
  :<|> "module"
    :> Capture "moduleName" T.Text
    :> ReqBody '[JSON] StepsModuleVersion
    :> Post '[JSON] ()
