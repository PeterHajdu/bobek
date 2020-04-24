module Message (Message (..)) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import ReceiveId (ReceiveId)

data Message = MkMessage
  { receiveId :: ReceiveId,
    routingKey :: Text,
    message :: ByteString
  }
  deriving stock (Eq, Show)
