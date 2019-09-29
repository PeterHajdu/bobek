module Destination(DestinationError, Destination(..), PublishResult(..))where

import Message(Message)
import Source
import ReceiveId(ReceiveId)

newtype DestinationError = MkDestinationError String deriving Show
data PublishResult = MkPublishResult {failed :: [ReceiveId], succeeded :: [ReceiveId]}

class Monad m => Destination m where
  publish :: [Message] -> m PublishResult

