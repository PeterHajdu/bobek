{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env(Env(..), App(..), runMover) where

import Mover
import Source
import Destination
import ReceiveId
import Message
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (MonadReader, liftIO, ask, MonadIO, runReaderT)

data Env = MkEnv
  { envPublish :: [Message] -> IO PublishResult
  , envReceive :: IO (Either SourceError Message)
  , envAcknowledge :: [ReceiveId] -> IO ()
  }

newtype App a = MkApp {run :: ReaderT Env IO a} deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance Destination App where
  publish msgs = do
    env <- ask
    liftIO $ (envPublish env) msgs

instance Source App where
  receive = do
    env <- ask
    liftIO $ envReceive env
  acknowledge ids = do
    env <- ask
    liftIO $ (envAcknowledge env) ids

runMover :: Env -> IO ()
runMover = runReaderT (run moveMessages)