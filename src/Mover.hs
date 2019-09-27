{-# LANGUAGE OverloadedStrings #-}

module Mover(moveMessages) where

import Destination
import Source
import Message()
import Data.Either (rights)

moveMessages :: (Source m, Destination m) => m ()
moveMessages = do
  maybeMessages <- receive 100
  let idsAndMessages = rights maybeMessages
  let messages = snd <$> idsAndMessages
  publishResult <- publish messages
  acknowledge $ succeeded publishResult
  return ()
