{-# LANGUAGE TemplateHaskell #-}

module Bobek.Destination
  ( DestinationError,
    Destination (..),
    PublishResult (..),
    publish,
  )
where

import Bobek.Message (Message)
import Bobek.ReceiveId (ReceiveId)
import Polysemy

newtype DestinationError = MkDestinationError String deriving stock (Show)

data PublishResult = MkPublishResult {failed :: [ReceiveId], succeeded :: [ReceiveId]}

data Destination m a where
  Publish :: [Message] -> Destination m PublishResult

makeSem ''Destination
