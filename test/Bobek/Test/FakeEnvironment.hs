{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bobek.Test.FakeEnvironment
  ( Environment (..),
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
import Polysemy
import Polysemy.State
import Safe (headDef, tailSafe)
import Prelude hiding (State, execState, get, put, runState)

runMoveMessages :: Environment -> Environment
runMoveMessages initial =
  moveMessages
    & runDestinationPure
    & runFilterPure
    & runLoggerPure
    & runSourcePure
    & execState initial
    & run

data Environment = MkEnv
  { toReceive :: [Either NoMessageReason Message],
    acknowledgedMessages :: [[ReceiveId]],
    published :: [[Message]],
    publishFun :: [Message] -> PublishResult,
    filterFun :: Message -> FilterActions
  }

runSourcePure ::
  Member (State Environment) m =>
  Sem (Source ': m) a ->
  Sem m a
runSourcePure = interpret $ \case
  Receive -> do
    oldEnv <- get
    let toRec = toReceive oldEnv
    let newEnv = oldEnv {toReceive = tailSafe toRec}
    put newEnv
    pure $ headDef (Left NMREmptyQueue) toRec
  Acknowledge ackIds -> do
    env@(MkEnv _ acks _ _ _) <- get
    put $ env {acknowledgedMessages = acks ++ [ackIds]}
    pure ()

runDestinationPure ::
  Member (State Environment) m =>
  Sem (Destination ': m) a ->
  Sem m a
runDestinationPure = interpret $ \case
  Publish publishedMessages -> do
    env@(MkEnv _ _ pubed pubFun _) <- get
    put $ env {published = pubed ++ [publishedMessages]}
    pure $ pubFun publishedMessages

runFilterPure ::
  Member (State Environment) m =>
  Sem (Filter ': m) a ->
  Sem m a
runFilterPure = interpret $ \case
  Action message -> do
    (MkEnv _ _ _ _ f) <- get
    pure (f message)

runLoggerPure :: Sem (Logger ': m) a -> Sem m a
runLoggerPure = interpret $ \case
  LogError _ -> pure ()
  LogDebug _ -> pure ()
