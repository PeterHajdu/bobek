{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bobek.Env
  ( Env (..),
    App (..),
    runMover,
    SourceFunctions (..),
  )
where

import Bobek.Destination
import Bobek.Filter
import Bobek.Log
import Bobek.Message
import Bobek.Mover
import Bobek.ReceiveId
import Bobek.Source
import Control.Monad.Reader (MonadIO, MonadReader, ask, asks, liftIO, runReaderT)
import Control.Monad.Trans.Reader (ReaderT)

data SourceFunctions = MkSourceFunctions (IO (Either NoMessageReason Message)) ([ReceiveId] -> IO ())

data Env = MkEnv
  { envPublish :: [Message] -> IO PublishResult,
    sourceFunctions :: SourceFunctions,
    envFilterAction :: Message -> IO FilterActions,
    logFunctions :: LogFunctions
  }

newtype App a = MkApp {run :: ReaderT Env IO a} deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance Destination App where
  publish msgs = do
    env <- ask
    liftIO $ envPublish env msgs

instance Source App where
  receive = do
    (MkSourceFunctions recv _) <- asks sourceFunctions
    liftIO recv
  acknowledge ids = do
    (MkSourceFunctions _ ack) <- asks sourceFunctions
    liftIO $ ack ids

instance Filter App where
  filterAction msg = do
    env <- ask
    liftIO $ envFilterAction env msg

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
