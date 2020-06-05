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

data PublishResult = MkPublishResult {failed :: [ReceiveId], succeeded :: [ReceiveId]} deriving (Show, Eq)

data Destination m a where
  Publish :: [Message] -> Destination m PublishResult

makeSem ''Destination
