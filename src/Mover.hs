module Mover(moveMessages) where

import Destination
import Source
import Message()
import Data.Either (rights)

moveMessages :: (Source m, Destination m) => m ()
moveMessages = do
  maybeMessage <- receive
  publishResult <- either (const $ return (MkPublishResult [] [])) (\message -> publish [message]) maybeMessage
  acknowledge $ succeeded publishResult
  return ()
