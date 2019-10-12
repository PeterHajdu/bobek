module ReceiveId(ReceiveId(..)) where

import Data.Word

newtype ReceiveId = MkReceiveId Word64 deriving (Eq, Show)
