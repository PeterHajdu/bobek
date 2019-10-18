module Filter(Filter(..), FilterAction(..), shouldAck) where

import Message

data FilterAction =
    Ack
  | Copy
  | CopyAndAck

shouldAck :: FilterAction -> Bool
shouldAck Copy = False
shouldAck _ = True

class Filter m where
  filterAction :: Message -> m FilterAction

