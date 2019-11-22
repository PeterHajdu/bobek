{-# LANGUAGE ScopedTypeVariables #-}
module Mover(moveMessages) where

import Destination
import Message
import Source
import Message()
import Filter
import ReceiveId
import Log(Logger(..))

import Data.Either(partitionEithers)
import Data.List(foldl')
import Control.Monad(unless, replicateM)

import Data.Set()
import qualified Data.Set as Set
import qualified Data.Text as T(concat, pack)

bulkSize :: Int
bulkSize = 1000

getMessages :: Source m => m (Either NoMessageReason [Message])
getMessages = do
  maybeMessages <- replicateM bulkSize receive
  let (errors, messages) = partitionEithers maybeMessages
  return $ if null messages
           then Left $ head errors
           else Right $ messages

runFilter :: Filter m => [Message] -> m [(FilterActions, Message)]
runFilter msgs = do
  filterActions <- traverse filterAction msgs
  return $ zip filterActions msgs

splitUpMessagesByAction :: [(FilterActions, Message)] -> (Set.Set ReceiveId, [Message], Set.Set ReceiveId)
splitUpMessagesByAction = foldl' splitter (Set.empty, [], Set.empty)
  where splitter (oldAck, oldPub, oldNoPub) (actions, msg) =
          let newAck = if (shouldAck actions) then Set.insert (receiveId msg) oldAck else oldAck
              newPub = if (shouldCopy actions) then msg:oldPub else oldPub
              newNoPub = if (shouldCopy actions) then oldNoPub else Set.insert (receiveId msg) oldNoPub
           in (newAck, newPub, newNoPub)

publishMessages :: Destination m => [Message] -> m (Set.Set ReceiveId)
publishMessages msgs = if null msgs
                       then return Set.empty
                       else (Set.fromList . succeeded) <$> publish msgs

publishAndAckMessages :: forall m.(Logger m, Source m, Destination m, Filter m) => [Message] -> m ()
publishAndAckMessages msgs = do
  actionsWithMessage <- runFilter msgs
  let (needsAck, needsPublish, doesNotNeedPublish) = splitUpMessagesByAction actionsWithMessage
  publishedIds <- publishMessages (reverse needsPublish)
  let canAck = Set.union publishedIds doesNotNeedPublish
  let toBeAcked = Set.intersection canAck needsAck
  _ <- unless (Set.null toBeAcked) $ acknowledge (Set.toList toBeAcked)
  logDebug $ T.concat [" needsPublish: ", (T.pack $ show $ length needsPublish),
                       " published: ", (T.pack $ show $ length needsPublish),
                       " needsAck: ", (T.pack $ show $ length needsAck),
                       " acked: ", (T.pack $ show $ length toBeAcked)]
  return ()

moveMessages :: (Logger m, Source m, Destination m, Filter m) => m ()
moveMessages = do
  logDebug "Reading messages."
  maybeMessages <- getMessages
  case maybeMessages of
    Left msg -> do
      logDebug $ T.concat ["Failed to read messages: ", (T.pack $ show msg)]
      return ()
    Right messages -> do
      logDebug "Publishing messages."
      publishAndAckMessages messages
      moveMessages
