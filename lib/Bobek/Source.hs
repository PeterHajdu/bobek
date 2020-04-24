module Bobek.Source (Source (..), NoMessageReason (..)) where

import Bobek.Message (Message)
import Bobek.ReceiveId (ReceiveId)

data NoMessageReason
  = NMRError String
  | NMREmptyQueue
  deriving stock (Show)

class Monad m => Source m where
  receive :: m (Either NoMessageReason Message)
  acknowledge :: [ReceiveId] -> m ()
