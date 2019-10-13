module Source(Source(..), SourceError(..)) where

import Message(Message)
import ReceiveId(ReceiveId)

newtype SourceError = MkSourceError String deriving Show

class Monad m => Source m where
  receive :: m (Either SourceError Message)
  acknowledge :: [ReceiveId] -> m ()

