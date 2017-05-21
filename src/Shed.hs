{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Shed
    ( startApp
    , app
    ) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Either (either)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Database.Redis
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified Shed.Model as Model
import qualified Shed.Repository as Repo
import Shed.Types (AppT(..))

--------------------------------------------------------------------------------
-- Api.
--------------------------------------------------------------------------------
type Api = "test" :> Get '[JSON] Model.Test

--------------------------------------------------------------------------------
-- Server setup.
--------------------------------------------------------------------------------
-- | Start our application.
startApp :: IO ()
startApp = run 8080 =<< mkApp
  where
    mkApp :: IO Application
    mkApp = do
      conn <- checkedConnect defaultConnectInfo
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
testH :: AppT IO Model.Test
testH = do
  pool <- ask
  e <- liftIO $ runRedis pool (Repo.getJson (Repo.Key "t"))
  either decodeError pure e
  where
    decodeError (Repo.JsonRetrievalError e) = customError err500 (T.unpack e)
    decodeError (Repo.JsonDecodeError e) = customError err404 (T.unpack e)


-- | Throw a custom ServantErr.
customError :: MonadError ServantErr m => ServantErr -> String -> m a
customError errF msg = throwError (errF
    { errBody = TL.encodeUtf8 . TL.fromStrict . T.pack $ msg
    })
