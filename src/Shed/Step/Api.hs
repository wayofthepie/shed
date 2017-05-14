{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Shed.Step.Api where

import Data.Aeson.TH
import qualified Data.Text as T
import Servant

data Step = Step
  { name :: T.Text
  , executables :: [Executable]
  } deriving (Eq, Show)

data VersionedStep = VersionedStep
  { version :: Int
  , step :: Step
  } deriving (Eq, Show)

data StepCreationSuccess = StepCreationSuccess
  { stepId :: T.Text
  , stepVersion :: Int
  } deriving (Eq, Show)

data Executable = Executable
  { cmd :: T.Text
  , args :: [T.Text]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Step)
$(deriveJSON defaultOptions ''VersionedStep)
$(deriveJSON defaultOptions ''Executable)
$(deriveJSON defaultOptions ''StepCreationSuccess)

type StepApi = "steps" :>
    ( ReqBody '[JSON] Step :> PostCreated '[JSON] StepCreationSuccess
    :<|> Capture "stepName" T.Text :> Get '[JSON] [VersionedStep]
    :<|> Capture "stepName" T.Text :> Capture "stepVersion" Int :> Get '[JSON] Step
    )
