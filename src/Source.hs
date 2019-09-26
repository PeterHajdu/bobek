module Source(Source(..), ReceiveId(..), SourceError(..)) where

import Message(Message)

newtype ReceiveId = MkReceiveId Int deriving Eq --todo: Integer?
newtype SourceError = MkSourceError String deriving Show

class Monad m => Source m where
  receive :: Int -> m [Either SourceError (ReceiveId, Message)]
  acknowledge :: [ReceiveId] -> m ()

