module Message(Message(..)) where

import Data.ByteString (ByteString)
import ReceiveId(ReceiveId)

data Message = MkMessage {receiveId :: ReceiveId, message :: ByteString} deriving (Eq, Show)
