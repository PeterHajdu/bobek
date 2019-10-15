module Mover(moveMessages) where

import Destination
import Source
import Message()

import Control.Monad (unless)

moveMessages :: (Source m, Destination m) => m ()
moveMessages = do
    stuff

stuff :: (Source m, Destination m) => m ()
stuff = do
  maybeMessage <- receive
  case maybeMessage of
    Left _ -> return ()
    Right message -> do
      publishResult <- publish [message]
      let ackable = succeeded publishResult
      unless (null ackable) $ acknowledge ackable
      stuff

