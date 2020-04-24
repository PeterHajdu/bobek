module Bobek.ReceiveId (ReceiveId (..)) where

import Data.Word

newtype ReceiveId = MkReceiveId Word64 deriving stock (Eq, Ord, Show)
