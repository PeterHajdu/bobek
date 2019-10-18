{-# LANGUAGE ScopedTypeVariables #-}
module Mover(moveMessages) where

import Destination
import Message
import Source
import Message()
import Filter

import Control.Monad (unless, void)

getMessages :: Source m => m (Either NoMessageReason [Message])
getMessages = do
  msg <- receive --todo: should use bulked publish
  return $ pure <$> msg

publishAndAckMessages :: forall m.(Source m, Destination m, Filter m) => [Message] -> m ()
publishAndAckMessages msgs = void $ traverse publishSingleMessage msgs --todo: should use bulked publish
  where publishSingleMessage :: Message -> m ()
        publishSingleMessage msg = do
          publishResult <- publish [msg]
          needsAck <- shouldAck <$> (filterAction msg)
          if needsAck then do
            let ackable = succeeded publishResult
            unless (null ackable) $ acknowledge ackable
          else return ()

moveMessages :: (Source m, Destination m, Filter m) => m ()
moveMessages = do
  maybeMessages <- getMessages
  case maybeMessages of
    Left _ -> return ()
    Right messages -> do
      publishAndAckMessages messages
      moveMessages
