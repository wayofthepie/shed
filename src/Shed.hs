{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Shed
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified Shelly as S

import Debug.Trace
import Step
import qualified Model


type API = "steps" :>
  ( ReqBody '[JSON] Step :> Post '[JSON] (Either T.Text (Key Model.Step))
  :<|> Capture "stepName" T.Text :> Get '[JSON] (Either T.Text Step)
  )

-- | Start our application.
startApp :: FilePath -> IO ()
startApp sqliteFile = run 8080 =<< mkApp sqliteFile
  where
    mkApp :: FilePath -> IO Application
    mkApp sqliteFile = do
      pool <- runStderrLoggingT $ createSqlitePool (T.pack sqliteFile) 5
      runSqlPool (runMigration Model.migrateAll) pool
      pure $ app pool

app :: ConnectionPool -> Application
app pool = serve api (server pool)

api :: Proxy API
api = Proxy

-- | Handle requests.
server :: ConnectionPool -> Server API
server pool = stepsPostH :<|> stepsGetWithIdH
  where
    stepsGetWithIdH stepName = liftIO $ getStepFromName pool stepName
    stepsPostH step = liftIO $ createStep pool step
