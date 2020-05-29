module Bobek.RabbitMqEnv (rabbitAcknowledge, createRabbitMqDestination) where

import Bobek.Destination
import qualified Bobek.Message as M
import Bobek.ReceiveId
import Bobek.Source
import Control.Arrow (left)
import Control.Exception (try)
import Control.Monad.Trans.Except (except)
import Data.IntSet (member)
import Data.List (partition)
import qualified Network.AMQP as AMQP (AMQPException, Ack (..), Channel, ConfirmationResult (..), ConnectionOpts, DeliveryMode (..), Envelope (..), Message (..), ackMsg, confirmSelect, getMsg, newMsg, openChannel, openConnection'', publishMsg, waitForConfirms)

catchAmqp :: IO a -> IO (Either AMQP.AMQPException a)
catchAmqp = try

rabbitPublish :: AMQP.Channel -> Text -> Maybe Text -> [M.Message] -> IO PublishResult
rabbitPublish channel exchange maybeRoutingKey messages = do
  publishResult <- traverse publishToRabbitMq messages
  confirmResult <- catchAmqp $ AMQP.waitForConfirms channel
  return $ either (const confirmFailed) (pubFromConfirmResult publishResult) confirmResult
  where
    publishToRabbitMq :: M.Message -> IO (Either ReceiveId (Int, ReceiveId))
    publishToRabbitMq msg = do
      let (rabbitMessage, routingKeyInMessage) = extractMessageRoutingKey msg
      let routingKey = fromMaybe routingKeyInMessage maybeRoutingKey
      result <- catchAmqp $ AMQP.publishMsg channel exchange routingKey rabbitMessage
      let rid = M.receiveId msg
      return $ bimap (const rid) (\seqNum -> (fromMaybe (error "Sequence number should be present always.") seqNum, rid)) result
    confirmFailed = MkPublishResult (M.receiveId <$> messages) []
    pubFromConfirmResult publishResult confirmResult =
      let confirmed = acked confirmResult
          (ok, nok) = partition (\(pid, _) -> member pid confirmed) (rights publishResult)
       in MkPublishResult (lefts publishResult ++ (snd <$> nok)) (snd <$> ok)
    acked :: AMQP.ConfirmationResult -> IntSet
    acked res = case res of
      AMQP.Complete (ok, _) -> ok
      AMQP.Partial _ -> error "Impossible."

messageFromRabbitMessage :: (AMQP.Message, AMQP.Envelope) -> M.Message
messageFromRabbitMessage (rabbitMessage, envelope) =
  let rid = MkReceiveId (AMQP.envDeliveryTag envelope)
      body = toStrict $ AMQP.msgBody rabbitMessage
   in M.MkMessage rid (AMQP.envRoutingKey envelope) body

extractMessageRoutingKey :: M.Message -> (AMQP.Message, Text)
extractMessageRoutingKey (M.MkMessage _ routingKey body) =
  (AMQP.newMsg {AMQP.msgBody = fromStrict body, AMQP.msgDeliveryMode = Just AMQP.Persistent}, routingKey)

rabbitReceive :: AMQP.Channel -> Text -> IO (Either NoMessageReason M.Message)
rabbitReceive channel queue = do
  maybeMessage <- catchAmqp $ AMQP.getMsg channel AMQP.Ack queue
  let msgWithFlattenedError = join $ bimap (NMRError . show) (maybeToRight NMREmptyQueue) maybeMessage
  return $ messageFromRabbitMessage <$> msgWithFlattenedError

rabbitAcknowledge :: (Word64 -> Bool -> IO ()) -> [ReceiveId] -> IO ()
rabbitAcknowledge ack = traverse_ ackMessage
  where
    ackMessage :: ReceiveId -> IO (Either AMQP.AMQPException ())
    ackMessage (MkReceiveId i) = catchAmqp $ ack i False

createAcknowledge :: AMQP.Channel -> [ReceiveId] -> IO ()
createAcknowledge channel = rabbitAcknowledge $ AMQP.ackMsg channel

createChannel :: AMQP.ConnectionOpts -> IO (Either Text AMQP.Channel)
createChannel connOpts = runExceptT $ do
  maybeConn <- liftIO $ catchAmqp $ AMQP.openConnection'' connOpts
  conn <- except $ left show maybeConn
  maybeChan <- liftIO $ catchAmqp $ AMQP.openChannel conn
  except $ left show maybeChan

createRabbitMqDestination :: AMQP.ConnectionOpts -> Text -> Maybe Text -> IO (Either Text ([M.Message] -> IO PublishResult))
createRabbitMqDestination connOpts exchange routingKey = runExceptT $ do
  maybeChan <- liftIO $ createChannel connOpts
  channel <- except $ left show maybeChan
  mightFail <- liftIO $ catchAmqp $ AMQP.confirmSelect channel False
  except $ left show mightFail
  return $ rabbitPublish channel exchange routingKey

-- createRabbitMqSource :: AMQP.ConnectionOpts -> Text -> IO (Either Text SourceFunctions)
-- createRabbitMqSource connOpts queue = do
--   maybeChan <- createChannel connOpts
--   return $ bimap show (\chan -> MkSourceFunctions (rabbitReceive chan queue) (createAcknowledge chan)) maybeChan

-- destinationToRabbitMq :: Member (Embed IO) m => Sem (Destination ': m) a -> Sem m a
-- destinationToRabbitMq = interpret $ \case
--   ReadFile file -> embed $
--     doesFileExist file >>= \case
--       False -> pure Nothing
--       True -> Just <$> IO.readFile file
