{-# LANGUAGE OverloadedStrings #-}

module Shed.Namespace where

import Data.Monoid
import qualified Data.Text as T

-- | A key namespace for Redis keys in Shed.
newtype ShedNs v = ShedNs T.Text deriving (Eq, Show)

newtype CompositionName = CompositionName T.Text deriving (Eq, Show)

newtype StageName = StageName T.Text deriving (Eq, Show)

newtype StepName = StepName T.Text deriving (Eq, Show)

data LaunchType = When | Expect deriving (Eq, Show)

-- | A `Monoid` for `ShedNs`. The identity is "shed".
instance Monoid (ShedNs v) where
  mempty = ShedNs "shed"
  (ShedNs a) `mappend` (ShedNs b) = ShedNs (a `T.append` ":" `T.append` b)

-- | Build a `ShedNs` from the given `Text`.
sns :: T.Text -> ShedNs v
sns = ShedNs

-- | The composition namespace, 'shed:composition'.
compositionNs :: ShedNs v
compositionNs = mempty <> sns "composition"

-- | Build a launch namespace from the given `CompositionName` and `LaunchType`.--
-- @
--  > launchNs (CompositionName "test") (When)
--  ShedNs "shed:composition:test:launch:when"
-- @
launchNs :: CompositionName -> LaunchType -> ShedNs [T.Text]
launchNs (CompositionName comName) lt
  | lt == When = launchNs' <> sns "when"
  | otherwise =  launchNs' <> sns "expect"
  where
    launchNs' = compositionNs <> sns comName <> sns "launch"

-- | Build a stages namespace from the given `CompositionName` and `StageName`.
-- @
--  > stagesNs (CompositionName "test") (StageName "test-stage")
--  ShedNs "shed:composition:test:stages:test-stage"
-- @
stagesNs :: CompositionName -> StageName -> ShedNs v
stagesNs (CompositionName comName) (StageName stageName) =
  compositionNs <> sns comName <> sns "stages" <> sns stageName

-- | Build a steps namespace from the given `CompositionName` and `StageName`.
-- @
--  > stepsNs  (CompositionName "test") (StageName "test-stage")
--  ShedNs "shed:composition:test:stages:test-stage:steps"
-- @
stepsNs :: CompositionName -> StageName -> ShedNs [StepName]
stepsNs cName sName = stagesNs cName sName <> sns "steps"
