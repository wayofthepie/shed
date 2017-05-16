{-# LANGUAGE OverloadedStrings #-}

module Shed.Namespace where

import Data.Monoid
import qualified Data.Text as T

-- | A key namespace for Redis keys in Shed. The phantom type _v_ defines
-- the expected type od the _value_ for this namespaced key.
newtype ShedNs v = ShedNs T.Text deriving (Eq, Show)

newtype CompositionName = CompositionName T.Text deriving (Eq, Show)

newtype StageName = StageName T.Text deriving (Eq, Show)

newtype StepName = StepName T.Text deriving (Eq, Show)

newtype ExpectedField = ExpectedField T.Text deriving (Eq, Show)

newtype EventName = EventName T.Text deriving (Eq, Show)

-- | A `Monoid` for `ShedNs`.
instance Monoid (ShedNs v) where
  mempty = ShedNs "shed"
  (ShedNs a) `mappend` (ShedNs b) = ShedNs (a `T.append` ":" `T.append` b)

-- | Build a `ShedNs` from the given `Text`.
sns :: T.Text -> ShedNs v
sns = ShedNs

--------------------------------------------------------------------------------
-- Composition
--------------------------------------------------------------------------------
-- | The composition namespace, _shed:composition_.
compositionNs :: ShedNs v
compositionNs = mempty <> sns "composition"

-- | Build a launch namespace from the given `CompositionName` and `LaunchType`.
--
-- @
--  > compositionLaunchNs (CompositionName "test") (When)
--  ShedNs "shed:composition:test:launch:when"
-- @
compositionLaunchNs :: CompositionName -> ShedNs v
compositionLaunchNs (CompositionName comName)  =
  compositionNs <> sns comName <> sns "launch"

-- | Build a _launch when_ namespace. This defines _when_ - afterreceiving  what
-- event - a composition should run.
--
-- @
--  > compositionLaunchWhenNs (CompositionName "test")
--  ShedNs "shed:composition:test:launch:when"
-- @
compositionLaunchWhenNs :: CompositionName -> ShedNs [EventName]
compositionLaunchWhenNs comName = compositionLaunchNs comName <> sns "when"

-- | Build a _launch expect_ namespace. This defines _what_ fieldnames are
-- expected in _all_ events listed in the _launch when_ namespace.
--
-- @
--  > compositionLaunchExpectNs (CompositionName "test")
--  ShedNs "shed:composition:test:launch:expect"
-- @
compositionLaunchExpectNs :: CompositionName -> ShedNs [ExpectedField]
compositionLaunchExpectNs comName = compositionLaunchNs comName <> sns "expect"

-- | Build a stages namespace from the given `CompositionName` and `StageName`.
--
-- @
--  > compositionStagesNs (CompositionName "test") (StageName "test-stage")
--  ShedNs "shed:composition:test:stages:test-stage"
-- @
compositionStagesNs :: CompositionName -> StageName -> ShedNs v
compositionStagesNs (CompositionName comName) (StageName stageName) =
  compositionNs <> sns comName <> sns "stages" <> sns stageName

-- | Build a steps namespace from the given `CompositionName` and `StageName`.
--
-- @
--  > compositionStepsNs (CompositionName "test") (StageName "test-stage")
--  ShedNs "shed:composition:test:stages:test-stage:steps"
-- @
compositionStepsNs :: CompositionName -> StageName -> ShedNs [StepName]
compositionStepsNs cName sName = compositionStagesNs cName sName <> sns "steps"


--------------------------------------------------------------------------------
-- Step
--------------------------------------------------------------------------------
