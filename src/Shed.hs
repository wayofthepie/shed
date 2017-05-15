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
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Shed.Types (AppT(..))

--------------------------------------------------------------------------------
-- Api.
--------------------------------------------------------------------------------
type Api = "test" :> Get '[JSON] T.Text

--------------------------------------------------------------------------------
-- Server setup.
--------------------------------------------------------------------------------
-- | Start our application.
startApp :: FilePath -> IO ()
startApp sqliteFile = run 8080 =<< mkApp
  where
    mkApp :: IO Application
    mkApp = pure $ app (T.pack "test")

runAppT :: T.Text -> AppT IO :~> Handler
runAppT pool = Nat (flip runReaderT pool . runApp)

app :: T.Text -> Application
app pool = serve api (readerServer pool)
  where
    api :: Proxy Api
    api = Proxy

readerServer :: T.Text -> Server Api
readerServer pool = enter (runAppT pool) readerServerT

-- | Combine our handlers.
readerServerT :: ServerT Api (AppT IO)
readerServerT = testH

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------
-- | Create a Step.
testH :: AppT IO T.Text
testH = pure (T.pack "Fuck ya")
