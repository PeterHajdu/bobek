module Bobek.Message (Message (..)) where

import Bobek.ReceiveId (ReceiveId)
import Data.ByteString (ByteString)
import Data.Text (Text)

data Message = MkMessage
  { receiveId :: ReceiveId,
    routingKey :: Text,
    message :: ByteString
  }
  deriving stock (Eq, Show)
