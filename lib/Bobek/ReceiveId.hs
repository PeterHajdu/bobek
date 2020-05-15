module Bobek.ReceiveId (ReceiveId (..)) where

newtype ReceiveId = MkReceiveId Word64 deriving stock (Eq, Ord, Show)
