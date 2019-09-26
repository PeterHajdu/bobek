{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FakeEnvironment(FakeEnvironment(..), Environment(..), runMoveMessages) where

import Message(Message)
import Mover
import Source
import Destination
import Control.Monad.State.Strict (State, execState)

runMoveMessages :: Environment -> Environment
runMoveMessages env = execState (run moveMessages) env

data Environment = MkEnv {
    toReceive :: [Either SourceError (ReceiveId, Message)]
  , published :: [Message]
  , publishResults :: [Either DestinationError ()]
}

newtype FakeEnvironment a = MkFakeEnvironment {run :: State Environment a} deriving (Functor, Applicative, Monad)

instance Source FakeEnvironment where
  receive = undefined
  acknowledge = undefined

instance Destination FakeEnvironment where
  publish = undefined
