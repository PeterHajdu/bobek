module Destination (DestinationError, Destination (..), PublishResult (..)) where

import Message (Message)
import ReceiveId (ReceiveId)

newtype DestinationError = MkDestinationError String deriving stock (Show)

data PublishResult = MkPublishResult {failed :: [ReceiveId], succeeded :: [ReceiveId]}

class Monad m => Destination m where
  publish :: [Message] -> m PublishResult
