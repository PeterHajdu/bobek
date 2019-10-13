{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FakeEnvironment(FakeEnvironment(..), Environment(..), runMoveMessages) where

import ReceiveId(ReceiveId)
import Message(Message)
import Mover
import Source
import Destination
import Control.Monad.State.Strict (State, execState)
import Control.Monad.State (MonadState, get, modify', put)

runMoveMessages :: Environment -> Environment
runMoveMessages env = execState (run moveMessages) env

data Environment = MkEnv {
    toReceive :: [Either SourceError Message]
  , acknowledgedMessages :: [ReceiveId]
  , published :: [Message]
  , publishResults :: PublishResult
}

newtype FakeEnvironment a = MkFakeEnvironment {run :: State Environment a} deriving (Functor, Applicative, Monad, MonadState Environment)

instance Source FakeEnvironment where
  receive = do
    env <- get
    return (head $ toReceive env)
  acknowledge ackIds = modify' (\env -> env {acknowledgedMessages = ackIds})

instance Destination FakeEnvironment where
  publish publishedMessages = do
    env <- get
    put $ env {published = publishedMessages}
    return $ publishResults env
