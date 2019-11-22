{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FakeEnvironment(FakeEnvironment(..), Environment(..), runMoveMessages) where

import ReceiveId(ReceiveId)
import Message(Message)
import Mover
import Source
import Destination
import Control.Monad.State.Strict (State, execState)
import Control.Monad.State (MonadState, get, put)
import Safe(tailSafe, headDef)
import Filter

runMoveMessages :: Environment -> Environment
runMoveMessages env = execState (run moveMessages) env

data Environment = MkEnv {
    toReceive :: [Either NoMessageReason Message]
  , acknowledgedMessages :: [[ReceiveId]]
  , published :: [[Message]]
  , publishFun :: [Message] -> PublishResult
  , filterFun :: Message -> FilterActions
}

newtype FakeEnvironment a = MkFakeEnvironment {run :: State Environment a} deriving (Functor, Applicative, Monad, MonadState Environment)

instance Source FakeEnvironment where
  receive = do
    oldEnv <- get
    let toRec = toReceive oldEnv
    let newEnv = oldEnv {toReceive = tailSafe toRec}
    put newEnv
    return (headDef (Left NMREmptyQueue) toRec)

  acknowledge ackIds = do
    env@(MkEnv _ acks _ _ _) <- get
    put $ env {acknowledgedMessages = acks++[ackIds]}

instance Destination FakeEnvironment where
  publish publishedMessages = do
    env@(MkEnv _ _ pubed pubFun _) <- get
    put $ env {published = pubed++[publishedMessages]}
    return $ pubFun publishedMessages

instance Filter FakeEnvironment where
  filterAction message = do
    (MkEnv _ _ _ _ f) <- get
    return (f message)

