{-# LANGUAGE OverloadedStrings #-}

module Mover(moveMessages) where

import Destination
import Source
import Message()
import Data.Either (rights)

moveMessages :: (Source m, Destination m) => m ()
moveMessages = do
  maybeMessages <- receive 1
  let messages = snd <$> rights maybeMessages
  _ <- publish messages
  return ()
