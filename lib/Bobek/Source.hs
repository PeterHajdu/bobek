{-# LANGUAGE TemplateHaskell #-}

module Bobek.Source
  ( reasonText,
    Source (..),
    NoMessageReason (..),
    receive,
    acknowledge,
  )
where

import Bobek.Message (Message)
import Bobek.ReceiveId (ReceiveId)
import Polysemy

data NoMessageReason
  = NMRError Text
  | NMREmptyQueue
  deriving stock (Eq, Show)

reasonText :: NoMessageReason -> Text
reasonText (NMRError msg) = msg
reasonText NMREmptyQueue = "Empty queue."

data Source m a where
  Receive :: Source m (Either NoMessageReason Message)
  Acknowledge :: [ReceiveId] -> Source m ()

makeSem ''Source
