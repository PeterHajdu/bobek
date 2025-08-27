module Bobek.Source (reasonText, Source (..), NoMessageReason (..)) where

import Bobek.Message (Message)
import Bobek.ReceiveId (ReceiveId)

data NoMessageReason
  = NMRError Text
  | NMREmptyQueue
  deriving stock (Eq, Show)

reasonText :: NoMessageReason -> Text
reasonText (NMRError msg) = msg
reasonText NMREmptyQueue = "Empty queue."

class (Monad m) => Source m where
  receive :: m (Either NoMessageReason Message)
  acknowledge :: [ReceiveId] -> m ()
