{-# LANGUAGE DeriveGeneric #-}

module Shed.Model where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Test = Test
  { test :: T.Text
  } deriving (Eq, Generic, Show)

instance ToJSON Test
instance FromJSON Test
