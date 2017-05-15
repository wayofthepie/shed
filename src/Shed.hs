{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Shed
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Redis
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified Shed.Repository as Repo
import Shed.Types (AppT(..))

--------------------------------------------------------------------------------
-- Api.
--------------------------------------------------------------------------------
type Api = "test" :> Get '[JSON] (Either T.Text T.Text)

--------------------------------------------------------------------------------
-- Server setup.
--------------------------------------------------------------------------------
-- | Start our application.
startApp :: IO ()
startApp = run 8080 =<< mkApp
  where
    mkApp :: IO Application
    mkApp = do
      conn <- connect defaultConnectInfo
      pure (app conn)

runAppT :: Connection -> AppT IO :~> Handler
runAppT pool = Nat (flip runReaderT pool . runApp)

app :: Connection -> Application
app pool = serve api (readerServer pool)
  where
    api :: Proxy Api
    api = Proxy

readerServer :: Connection -> Server Api
readerServer pool = enter (runAppT pool) readerServerT

-- | Combine our handlers.
readerServerT :: ServerT Api (AppT IO)
readerServerT = testH

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------
-- | Create a Step.
testH :: AppT IO (Either T.Text T.Text)
testH = do
  pool <- ask
  liftIO $ runRedis pool (Repo.storeTextAt (T.pack "test") (T.pack "test"))
