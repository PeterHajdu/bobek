module Source(Source(..), NoMessageReason(..)) where

import Message(Message)
import ReceiveId(ReceiveId)

data NoMessageReason =
    NMRError String
  | NMREmptyQueue
  deriving Show

class Monad m => Source m where
  receive :: m (Either NoMessageReason Message)
  acknowledge :: [ReceiveId] -> m ()

