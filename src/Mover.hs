module Mover(moveMessages) where

import Destination
import Source
import Message

moveMessages :: (Source m, Destination m) => m ()
moveMessages = do
  messages <- receive 1
  publish [MkMessage]
  return ()
