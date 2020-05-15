module Bobek.Mover
  ( moveMessages,
  )
where

import Bobek.Destination
import Bobek.Filter
import Bobek.Log (Logger (..))
import Bobek.Message
import Bobek.ReceiveId
import Bobek.Source
import Data.Set ()
import qualified Data.Set as Set
import qualified Data.Text as T (concat)

bulkSize :: Int
bulkSize = 1000

getMessages :: (Logger m, Source m) => m (Maybe [Message])
getMessages = do
  maybeMessages <- replicateM bulkSize receive
  let (errors, messages) = partitionEithers maybeMessages
  traverse_ (logError . reasonText) errors
  return $
    if null messages
      then Nothing
      else Just messages

runFilter :: Filter m => [Message] -> m [(FilterActions, Message)]
runFilter msgs = do
  filterActions <- traverse filterAction msgs
  pure $ zip filterActions msgs

splitUpMessagesByAction :: [(FilterActions, Message)] -> (Set.Set ReceiveId, [Message], Set.Set ReceiveId)
splitUpMessagesByAction = foldl' splitter (Set.empty, [], Set.empty)
  where
    splitter (oldAck, oldPub, oldNoPub) (actions, msg) =
      let newAck = if shouldAck actions then Set.insert (receiveId msg) oldAck else oldAck
          newPub = if shouldCopy actions then msg : oldPub else oldPub
          newNoPub = if shouldCopy actions then oldNoPub else Set.insert (receiveId msg) oldNoPub
       in (newAck, newPub, newNoPub)

publishMessages :: Destination m => [Message] -> m (Set.Set ReceiveId)
publishMessages msgs =
  if null msgs
    then return Set.empty
    else Set.fromList . succeeded <$> publish msgs

--todo: remove forall and extension
publishAndAckMessages :: (Logger m, Source m, Destination m, Filter m) => [Message] -> m ()
publishAndAckMessages msgs = do
  actionsWithMessage <- runFilter msgs
  let (needsAck, needsPublish, doesNotNeedPublish) = splitUpMessagesByAction actionsWithMessage
  publishedIds <- publishMessages (reverse needsPublish)
  let canAck = Set.union publishedIds doesNotNeedPublish
  let toBeAcked = Set.intersection canAck needsAck
  unless (Set.null toBeAcked) $ acknowledge (Set.toList toBeAcked)
  logDebug $
    T.concat
      [ " needsPublish: ",
        show . length $ needsPublish,
        " published: ",
        show . length $ publishedIds,
        " needsAck: ",
        show . length $ needsAck,
        " acked: ",
        show . length $ toBeAcked
      ]
  return ()

moveMessages :: (Logger m, Source m, Destination m, Filter m) => m ()
moveMessages = do
  logDebug "Reading messages."
  maybeMessages <- getMessages
  case maybeMessages of
    Nothing -> return ()
    Just messages -> do
      logDebug "Publishing messages."
      publishAndAckMessages messages
      moveMessages
