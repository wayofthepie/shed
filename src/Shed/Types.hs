{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shed.Types where
import Control.Monad.Except
import Control.Monad.Reader
import Database.Redis (Connection(..))
import Data.Text as T
import Servant

-- | Our applications monad transformer.
newtype AppT m a = AppT
  { runApp :: ReaderT Connection (ExceptT ServantErr m) a
  } deriving
      ( Monad
      , Functor
      , Applicative
      , MonadReader Connection
      , MonadIO
      , MonadError ServantErr
      )
