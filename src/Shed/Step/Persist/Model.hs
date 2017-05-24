{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Shed.Step.Persist.Model where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

newtype ModuleNs = ModuleNs T.Text deriving (Eq, FromJSON, Generic, ToJSON, Show)


data ModuleVersion = ModuleVersion
  { name :: T.Text
  , steps :: [Step]
  , version :: T.Text
  } deriving (Eq, FromJSON, Generic, ToJSON, Show)

data Step = Step
  { name :: T.Text
  , params :: [StepParam]
  , executables :: [Executable]
  } deriving (Eq, FromJSON, Generic, ToJSON, Show)

data StepParam = StepParam
  { key :: T.Text
  , value :: T.Text
  } deriving (Eq, FromJSON, Generic, ToJSON, Show)

data Executable = Executable
  { cmd :: T.Text
  , args :: [T.Text]
  } deriving (Eq, FromJSON, Generic, ToJSON, Show)
