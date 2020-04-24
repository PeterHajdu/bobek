module Bobek.Destination (DestinationError, Destination (..), PublishResult (..)) where

import Bobek.Message (Message)
import Bobek.ReceiveId (ReceiveId)

newtype DestinationError = MkDestinationError String deriving stock (Show)

data PublishResult = MkPublishResult {failed :: [ReceiveId], succeeded :: [ReceiveId]}

class Monad m => Destination m where
  publish :: [Message] -> m PublishResult
