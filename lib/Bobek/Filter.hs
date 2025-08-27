module Bobek.Filter (Filter (..), FilterAction (..), FilterActions (..), shouldAck, shouldCopy) where

import Bobek.Message

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

class (Monad m) => Filter m where
  filterAction :: Message -> m FilterActions
