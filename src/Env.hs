{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env(Env(..), App(..), runMover, SourceFunctions(..)) where

import Log
import Mover
import Source
import Destination
import ReceiveId
import Message
import Filter
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (MonadReader, liftIO, ask, asks, MonadIO, runReaderT)

data SourceFunctions = MkSourceFunctions (IO (Either NoMessageReason Message)) ([ReceiveId] -> IO ())

data Env = MkEnv
  { envPublish :: [Message] -> IO PublishResult
  , sourceFunctions :: SourceFunctions
  , envFilterAction :: Message -> IO FilterActions
  , logFunctions :: LogFunctions
  }

newtype App a = MkApp {run :: ReaderT Env IO a} deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance Destination App where
  publish msgs = do
    env <- ask
    liftIO $ (envPublish env) msgs

instance Source App where
  receive = do
    (MkSourceFunctions recv  _) <- asks sourceFunctions
    liftIO recv
  acknowledge ids = do
    (MkSourceFunctions _ ack) <- asks sourceFunctions
    liftIO $ ack ids

instance Filter App where
  filterAction msg = do
    env <- ask
    liftIO $ (envFilterAction env) msg

instance Logger App where
  logError msg = do
    env <- ask
    let errorF = errorFunction . logFunctions $ env
    liftIO $ errorF msg

  logDebug msg = do
    env <- ask
    let debugF = debugFunction . logFunctions $ env
    liftIO $ debugF msg

runMover :: Env -> IO ()
runMover = runReaderT (run moveMessages)
