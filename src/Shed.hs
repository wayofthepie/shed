{-# LANGUAGE DataKinds #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators   #-}
module Shed
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson
import Data.Aeson.TH
import Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified Shelly as S

import qualified Model

data Exec = Exec
  { cmd :: T.Text
  , args :: T.Text
  } deriving (Eq, Show)

newtype ExecResult = ExecResult
  { result :: T.Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Exec)
$(deriveJSON defaultOptions ''ExecResult)

type API = "exec" :> ReqBody '[JSON] Exec  :> Post '[JSON] ExecResult

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

server :: ConnectionPool -> Server API
server pool = execH
  where
    execH cmd = liftIO $ exec cmd

exec :: Exec -> IO ExecResult
exec (Exec cmd args) = S.shelly $ S.verbosely $
  S.run (S.fromText cmd) [args] >>= \r -> pure $ ExecResult r
