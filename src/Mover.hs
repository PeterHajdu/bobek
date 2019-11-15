{-# LANGUAGE ScopedTypeVariables #-}
module Mover(moveMessages) where

import Destination
import Message
import Source
import Message()
import Filter

import Data.Either (partitionEithers)
import Control.Monad (unless, replicateM)

bulkSize :: Int
bulkSize = 1000

getMessages :: Source m => m (Either NoMessageReason [Message])
getMessages = do
  maybeMessages <- replicateM bulkSize receive
  let (errors, messages) = partitionEithers maybeMessages
  return $ if null messages
           then Left $ head errors
           else Right $ messages

publishAndAckMessages :: forall m.(Source m, Destination m, Filter m) => [Message] -> m ()
publishAndAckMessages msgs = do
  publishResult <- publish msgs
  filterActions <- traverse filterAction msgs
  let actionWithId = zip filterActions (receiveId <$> msgs)
  let needsAck = filter (shouldAck . fst) actionWithId
  let toBeAcked = filter (\i -> elem i $ succeeded publishResult) (snd <$> needsAck)
  _ <- unless (null toBeAcked) $ acknowledge toBeAcked
  return ()

moveMessages :: (Source m, Destination m, Filter m) => m ()
moveMessages = do
  maybeMessages <- getMessages
  case maybeMessages of
    Left _ -> return ()
    Right messages -> do
      publishAndAckMessages messages
      moveMessages
