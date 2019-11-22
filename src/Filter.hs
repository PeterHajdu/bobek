module Filter(Filter(..), FilterAction(..), FilterActions) where

import Message

type FilterActions = [FilterAction]

data FilterAction =
    Ack
  | Copy
  deriving (Eq, Show)

class Monad m => Filter m where
  filterAction :: Message -> m FilterActions

