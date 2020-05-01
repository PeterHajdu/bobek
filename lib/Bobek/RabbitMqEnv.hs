module Bobek.RabbitMqEnv (createRabbitMqSource, createRabbitMqDestination) where

import Bobek.Destination
import Bobek.Env (SourceFunctions (..))
import qualified Bobek.Message as M
import Bobek.ReceiveId
import Bobek.Source
import Bobek.Util (tshow)
import Control.Arrow (left)
import Control.Exception (try)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except, runExceptT)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Either (lefts, rights)
import Data.Foldable (traverse_)
import Data.IntSet (IntSet, member)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Network.AMQP as AMQP (AMQPException, Ack (..), Channel, ConfirmationResult (..), ConnectionOpts, DeliveryMode (..), Envelope (..), Message (..), ackMsg, confirmSelect, getMsg, newMsg, openChannel, openConnection'', publishMsg, waitForConfirms)

catchAmqp :: IO a -> IO (Either AMQP.AMQPException a)
catchAmqp = try

rabbitPublish :: AMQP.Channel -> T.Text -> Maybe T.Text -> [M.Message] -> IO PublishResult
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

extractMessageRoutingKey :: M.Message -> (AMQP.Message, T.Text)
extractMessageRoutingKey (M.MkMessage _ routingKey body) =
  (AMQP.newMsg {AMQP.msgBody = fromStrict body, AMQP.msgDeliveryMode = Just AMQP.Persistent}, routingKey)

rabbitReceive :: AMQP.Channel -> T.Text -> IO (Either NoMessageReason M.Message)
rabbitReceive channel queue = do
  maybeMessage <- catchAmqp $ AMQP.getMsg channel AMQP.Ack queue
  let msgWithFlattenedError = join $ bimap (NMRError . show) (maybe (Left NMREmptyQueue) Right) maybeMessage
  return $ messageFromRabbitMessage <$> msgWithFlattenedError

rabbitAcknowledge :: AMQP.Channel -> [ReceiveId] -> IO ()
rabbitAcknowledge channel = traverse_ ackMessage
  where
    ackMessage :: ReceiveId -> IO (Either AMQP.AMQPException ())
    ackMessage (MkReceiveId i) = catchAmqp $ AMQP.ackMsg channel i False

createChannel :: AMQP.ConnectionOpts -> IO (Either T.Text AMQP.Channel)
createChannel connOpts = runExceptT $ do
  maybeConn <- liftIO $ catchAmqp $ AMQP.openConnection'' connOpts
  conn <- except $ left tshow maybeConn
  maybeChan <- liftIO $ catchAmqp $ AMQP.openChannel conn
  except $ left tshow maybeChan

createRabbitMqSource :: AMQP.ConnectionOpts -> T.Text -> IO (Either T.Text SourceFunctions)
createRabbitMqSource connOpts queue = do
  maybeChan <- createChannel connOpts
  return $ bimap tshow (\chan -> MkSourceFunctions (rabbitReceive chan queue) (rabbitAcknowledge chan)) maybeChan

createRabbitMqDestination :: AMQP.ConnectionOpts -> T.Text -> Maybe T.Text -> IO (Either T.Text ([M.Message] -> IO PublishResult))
createRabbitMqDestination connOpts exchange routingKey = runExceptT $ do
  maybeChan <- liftIO $ createChannel connOpts
  channel <- except $ left tshow maybeChan
  mightFail <- liftIO $ catchAmqp $ AMQP.confirmSelect channel False
  except $ left tshow mightFail
  return $ rabbitPublish channel exchange routingKey
