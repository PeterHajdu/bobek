module Bobek.Message (Message (..)) where

import Bobek.ReceiveId (ReceiveId)

data Message = MkMessage
  { receiveId :: ReceiveId,
    routingKey :: Text,
    message :: ByteString
  }
  deriving stock (Eq, Show)
