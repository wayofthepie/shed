{-# LANGUAGE FlexibleContexts #-}

module Shed.Error where

import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Servant

-- | Throw a custom ServantErr.
customError :: MonadError ServantErr m => ServantErr -> String -> m a
customError errF msg = throwError (errF
    { errBody = TL.encodeUtf8 . TL.fromStrict . T.pack $ msg
    })
