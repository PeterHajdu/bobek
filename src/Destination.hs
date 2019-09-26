module Destination(DestinationError, Destination(..))where

import Message(Message)

newtype DestinationError = MkDestinationError String deriving Show

class Monad m => Destination m where
  publish :: [Message] -> m ([Either DestinationError ()])

