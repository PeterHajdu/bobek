{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env(Env(..), App(..), runMover) where

import Mover
import Source
import Destination
import ReceiveId
import Message
import Filter
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (MonadReader, liftIO, ask, MonadIO, runReaderT)

data Env = MkEnv
  { envPublish :: [Message] -> IO PublishResult
  , envReceive :: IO (Either NoMessageReason Message)
  , envAcknowledge :: [ReceiveId] -> IO ()
  , envFilterAction :: Message -> IO FilterAction
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

instance Filter App where
  filterAction msg = do
    env <- ask
    liftIO $ (envFilterAction env) msg

runMover :: Env -> IO ()
runMover = runReaderT (run moveMessages)
