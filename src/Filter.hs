module Filter(Filter(..), FilterAction(..), FilterActions(..), shouldAck, shouldCopy) where

import Message

newtype FilterActions = MkFilterActions [FilterAction]

instance Semigroup FilterActions where
  (MkFilterActions l) <> (MkFilterActions r) = MkFilterActions (l <> r)

instance Monoid FilterActions where
  mempty = MkFilterActions []

data FilterAction =
    Ack
  | Copy
  deriving (Eq, Show)

shouldAck :: FilterActions -> Bool
shouldAck (MkFilterActions actions) = elem Ack actions

shouldCopy :: FilterActions -> Bool
shouldCopy (MkFilterActions actions) = elem Copy actions

class Monad m => Filter m where
  filterAction :: Message -> m FilterActions

