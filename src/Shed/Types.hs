{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shed.Types where
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Trans.Control
  ( MonadBaseControl(..)
  , MonadTransControl(..)
  , ComposeSt
  , defaultLiftBaseWith
  , defaultRestoreM
  )
import Control.Monad.Except
import Control.Monad.Reader
import Database.Persist.Sql
import Servant

-- | Our applications monad transformer.
newtype AppT m a = App
  { runApp :: ReaderT ConnectionPool (ExceptT ServantErr m) a
  } deriving
      ( Monad
      , Functor
      , Applicative
      , MonadReader ConnectionPool
      , MonadIO
      , MonadError ServantErr
      )

instance MonadTrans AppT where
  lift = App . lift . lift

instance MonadBase b m => MonadBase b (AppT m) where
  liftBase = liftBaseDefault

instance MonadTransControl AppT where
  type StT AppT a = StT (ExceptT ServantErr) (StT (ReaderT ConnectionPool) a)
  liftWith f = App $ liftWith $ \run ->
    liftWith $ \run' ->
      f (run' . run . runApp)
  restoreT = App . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (AppT m) where
  type StM (AppT m) a = ComposeSt AppT m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM
