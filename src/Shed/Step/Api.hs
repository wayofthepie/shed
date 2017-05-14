{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Shed.Step.Api where

import Data.Aeson.TH
import qualified Data.Text as T
import Servant

data Step = Step
  { name :: T.Text
  , execute :: [Executable]
  } deriving (Eq, Show)

newtype StepCreationSuccess = StepCreationSuccess
  {  stepId :: T.Text
  } deriving (Eq, Show)

data Executable = Executable
  { cmd :: T.Text
  , args :: [T.Text]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Step)
$(deriveJSON defaultOptions ''Executable)
$(deriveJSON defaultOptions ''StepCreationSuccess)

type StepApi = "steps" :>
    ( ReqBody '[JSON] Step :> PostCreated '[JSON] StepCreationSuccess
    :<|> Capture "stepName" T.Text :> Get '[JSON] Step
    )
