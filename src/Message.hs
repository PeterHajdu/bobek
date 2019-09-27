module Message(Message(..)) where

import Data.ByteString (ByteString)

data Message = MkMessage {message :: ByteString} deriving (Eq, Show)
