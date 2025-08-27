{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bobek.Test.FakeEnvironment
  ( FakeEnvironment (..),
    Environment (..),
    runMoveMessages,
  )
where

import Bobek.Destination
import Bobek.Filter
import Bobek.Log (Logger (..))
import Bobek.Message (Message)
import Bobek.Mover
import Bobek.ReceiveId (ReceiveId)
import Bobek.Source
import Safe (headDef, tailSafe)

runMoveMessages :: Environment -> Environment
runMoveMessages = execState (run moveMessages)

data Environment = MkEnv
  { toReceive :: [Either NoMessageReason Message],
    acknowledgedMessages :: [[ReceiveId]],
    published :: [[Message]],
    publishFun :: [Message] -> PublishResult,
    filterFun :: Message -> FilterActions,
    logged :: [Text]
  }

newtype FakeEnvironment a = MkFakeEnvironment {run :: State Environment a} deriving newtype (Functor, Applicative, Monad, MonadState Environment)

instance Source FakeEnvironment where
  receive = do
    oldEnv <- get
    let toRec = toReceive oldEnv
    let newEnv = oldEnv {toReceive = tailSafe toRec}
    put newEnv
    return (headDef (Left NMREmptyQueue) toRec)

  acknowledge ackIds = do
    env@(MkEnv _ acks _ _ _ _) <- get
    put $ env {acknowledgedMessages = acks ++ [ackIds]}

instance Destination FakeEnvironment where
  publish publishedMessages = do
    env@(MkEnv _ _ pubed pubFun _ _) <- get
    put $ env {published = pubed ++ [publishedMessages]}
    return $ pubFun publishedMessages

instance Filter FakeEnvironment where
  filterAction message = do
    (MkEnv _ _ _ _ f _) <- get
    return (f message)

instance Logger FakeEnvironment where
  logError msg = do
    env@(MkEnv _ _ _ _ _ loggedMessages) <- get
    put $ env { logged = loggedMessages ++ [msg]}
  logDebug = logError
