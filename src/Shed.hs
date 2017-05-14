{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Shed
    ( startApp
    , app
    ) where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist ()
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH ()
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Shed.Step
import Shed.Types (AppT(..))

--------------------------------------------------------------------------------
-- Api.
--------------------------------------------------------------------------------
type Api = StepApi

--------------------------------------------------------------------------------
-- Server setup.
--------------------------------------------------------------------------------
-- | Start our application.
startApp :: FilePath -> IO ()
startApp sqliteFile = run 8080 =<< mkApp
  where
    mkApp :: IO Application
    mkApp = do
      pool <- runStderrLoggingT $ createSqlitePool (T.pack sqliteFile) 5
      runSqlPool (runMigration migrateAll) pool
      pure $ app pool

runAppT :: ConnectionPool -> AppT IO :~> Handler
runAppT pool = Nat (flip runReaderT pool . runApp)

app :: ConnectionPool -> Application
app pool = serve api (readerServer pool)
  where
    api :: Proxy Api
    api = Proxy

readerServer :: ConnectionPool -> Server Api
readerServer pool = enter (runAppT pool) readerServerT

-- | Combine our handlers.
readerServerT :: ServerT Api (AppT IO)
readerServerT = stepsPostH
  :<|> stepsGetFromNameH
  :<|> stepsGetFromNameVersionIdH

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------
-- | Create a Step.
stepsPostH :: Step -> AppT IO StepCreationSuccess
stepsPostH = createStep

-- | Get all Steps with the given name.
stepsGetFromNameH :: T.Text -> AppT IO [VersionedStep]
stepsGetFromNameH = getStepsForName

-- | Get a Step with the given name and version.
stepsGetFromNameVersionIdH :: T.Text -> Int -> AppT IO Step
stepsGetFromNameVersionIdH = getStepFromNameAndVersion
