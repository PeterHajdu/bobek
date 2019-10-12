module Mover(moveMessages) where

import Destination
import Source
import Message()
import Data.Either (rights)

moveMessages :: (Source m, Destination m) => m ()
moveMessages = do
  maybeMessages <- receive 100
  publishResult <- publish $ rights maybeMessages
  acknowledge $ succeeded publishResult
  return ()
