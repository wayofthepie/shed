{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Shed
    ( startApp
    , app
    ) where

import qualified Control.Category as C
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import Control.Monad.Trans.Either (EitherT(..), left, right)
import Data.Aeson
import Data.Aeson.TH
import Data.Either (either)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy as TL
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

--------------------------------------------------------------------------------
-- Application monad.
--------------------------------------------------------------------------------
newtype App a = App
  { runApp :: ReaderT ConnectionPool (ExceptT ServantErr IO) a
  } deriving
      ( Monad
      , Functor
      , Applicative
      , MonadReader ConnectionPool
      , MonadIO
      , MonadError ServantErr
      )

--------------------------------------------------------------------------------
-- Api.
--------------------------------------------------------------------------------
type Api = "steps" :>
  ( ReqBody '[JSON] Step :> PostCreated '[JSON] T.Text
  :<|> Capture "stepName" T.Text :> Get '[JSON] Step
  )

--------------------------------------------------------------------------------
-- Server setup.
--------------------------------------------------------------------------------
-- | Start our application.
startApp :: FilePath -> IO ()
startApp sqliteFile = run 8080 =<< mkApp sqliteFile
  where
    mkApp :: FilePath -> IO Application
    mkApp sqliteFile = do
      pool <- runStderrLoggingT $ createSqlitePool (T.pack sqliteFile) 5
      runSqlPool (runMigration Model.migrateAll) pool
      pure $ app pool

runAppT :: ConnectionPool -> App :~> Handler
runAppT pool = Nat (flip runReaderT pool . runApp)

app :: ConnectionPool -> Application
app pool = serve api (readerServer pool)
  where
    api :: Proxy Api
    api = Proxy

readerServer :: ConnectionPool -> Server Api
readerServer pool = enter (runAppT pool) readerServerT

-- | Combine our handlers.
readerServerT :: ServerT Api App
readerServerT = stepsPostH :<|> stepsGetWithIdH

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------
-- | Create a Step.
stepsPostH :: Step -> App T.Text
stepsPostH step = do
  pool <- ask
  eitherStep <- liftIO $ createStep pool step
  case eitherStep of
    Right stepId -> pure stepId
    Left e -> throwError (err409
      { errBody = TE.encodeUtf8 . TL.fromStrict $ e
      })

-- | Get a step with the given ID.
stepsGetWithIdH :: T.Text -> App Step
stepsGetWithIdH stepName = do
  pool <- ask
  eitherStep <- liftIO $ getStepFromName pool stepName
  case eitherStep of
    Right step -> pure step
    Left e -> throwError (err404
      { errBody = TE.encodeUtf8 . TL.fromStrict $ e
      })
