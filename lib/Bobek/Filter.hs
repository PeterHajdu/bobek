{-# LANGUAGE TemplateHaskell #-}

module Bobek.Filter
  ( Filter (..),
    action,
    FilterAction (..),
    FilterActions (..),
    shouldAck,
    shouldCopy,
    noopInterpreter,
  )
where

import Bobek.Message
import Polysemy

newtype FilterActions = MkFilterActions [FilterAction]

instance Semigroup FilterActions where
  (MkFilterActions l) <> (MkFilterActions r) = MkFilterActions (l <> r)

instance Monoid FilterActions where
  mempty = MkFilterActions []

data FilterAction
  = Ack
  | Copy
  deriving stock (Eq, Show)

shouldAck :: FilterActions -> Bool
shouldAck (MkFilterActions actions) = Ack `elem` actions

shouldCopy :: FilterActions -> Bool
shouldCopy (MkFilterActions actions) = Copy `elem` actions

data Filter m a where
  Action :: Message -> Filter m FilterActions

makeSem ''Filter

noopInterpreter ::
  Members '[(Embed IO)] m =>
  Sem (Filter ': m) a ->
  Sem m a
noopInterpreter = interpret $ \case
  Action _ -> pure mempty
