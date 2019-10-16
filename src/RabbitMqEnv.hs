module RabbitMqEnv(createRabbitMqSource, createRabbitMqDestination) where

import Message
import Source
import Destination
import ReceiveId

import Data.List (partition)
import Data.IntSet (IntSet, member)
import Data.Maybe (fromJust)
import Data.Either(lefts, rights)
import qualified Data.Text as T
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Network.AMQP as AMQP(AMQPException, Connection, ConnectionOpts, publishMsg, DeliveryMode(..), newMsg, openChannel, openConnection'', Channel, getMsg, Message(..), Ack(..), ackMsg, Envelope(..), confirmSelect, waitForConfirms, ConfirmationResult(..))
import Control.Monad(void, join)
import Control.Monad.IO.Class(liftIO)

import Control.Monad.Trans.Except(runExceptT, except)
import Control.Exception(try)

import Control.Arrow(left)
import Data.Bifunctor(bimap)

rabbitPublish :: AMQP.Channel -> T.Text -> T.Text -> [Message] -> IO PublishResult
rabbitPublish channel exchange routingkey messages = do
  publishResult <- traverse (\msg -> publishToRabbitMq msg) messages
  confirmed <- acked <$> AMQP.waitForConfirms channel --todo: handle exceptions
  let (ok, nok) = partition (\(pid, _) -> member pid confirmed) (rights publishResult)
  return $ MkPublishResult (lefts publishResult ++ (snd <$> nok)) (snd <$> ok)
  where publishToRabbitMq :: Message -> IO (Either ReceiveId (Int, ReceiveId))
        publishToRabbitMq msg = do
          result <- try $ AMQP.publishMsg channel exchange routingkey (rabbitMessageFromMessage msg) :: IO (Either AMQP.AMQPException (Maybe Int))
          let rid = receiveId msg
          return $ bimap (const rid) (\seqNum -> (fromJust seqNum, rid)) result

        acked :: AMQP.ConfirmationResult -> IntSet
        acked res = case res of
                      AMQP.Complete (ok, _) -> ok
                      AMQP.Partial (ok, _, _) -> ok

messageFromRabbitMessage :: (AMQP.Message, AMQP.Envelope) -> Message
messageFromRabbitMessage (rabbitMessage, envelope) =
  let rid = MkReceiveId (AMQP.envDeliveryTag envelope)
      body = toStrict $ AMQP.msgBody rabbitMessage
   in MkMessage rid (AMQP.envRoutingKey envelope) body

rabbitMessageFromMessage :: Message -> AMQP.Message
rabbitMessageFromMessage (MkMessage _ _ body) =
  AMQP.newMsg {AMQP.msgBody = fromStrict body, AMQP.msgDeliveryMode = Just AMQP.Persistent}

rabbitReceive :: AMQP.Channel -> T.Text -> IO (Either NoMessageReason Message)
rabbitReceive channel queue = do
  maybeMessage <- (try $ AMQP.getMsg channel AMQP.Ack queue :: IO (Either AMQP.AMQPException (Maybe (AMQP.Message, AMQP.Envelope))))
  let msgWithFlattenedError = join $ bimap (NMRError . show) (maybe (Left NMREmptyQueue) Right) maybeMessage
  return $ messageFromRabbitMessage <$> msgWithFlattenedError

rabbitAcknowledge :: AMQP.Channel -> [ReceiveId] -> IO ()
rabbitAcknowledge channel ids = void $ traverse ackMessage ids
  where ackMessage :: ReceiveId -> IO (Either AMQP.AMQPException ())
        ackMessage (MkReceiveId i) = try $ AMQP.ackMsg channel i False :: IO (Either AMQP.AMQPException ())

createChannel :: AMQP.ConnectionOpts -> IO (Either String AMQP.Channel)
createChannel connOpts = runExceptT $ do
  maybeConn <- liftIO $ (try $ AMQP.openConnection'' connOpts :: IO (Either AMQP.AMQPException AMQP.Connection))
  conn <- except $ left show maybeConn
  maybeChan <- liftIO $ (try $ AMQP.openChannel conn :: IO (Either AMQP.AMQPException AMQP.Channel))
  except $ left show maybeChan

createRabbitMqSource :: AMQP.ConnectionOpts -> T.Text -> IO (Either String (IO (Either NoMessageReason Message), [ReceiveId] -> IO ()))
createRabbitMqSource connOpts queue = do
  maybeChan <- createChannel connOpts
  return $ bimap show (\chan -> (rabbitReceive chan queue, rabbitAcknowledge chan)) maybeChan

createRabbitMqDestination :: AMQP.ConnectionOpts -> T.Text -> T.Text -> IO (Either String ([Message] -> IO PublishResult))
createRabbitMqDestination connOpts exchange routingKey = runExceptT $ do
  maybeChan <- liftIO $ createChannel connOpts
  channel <- except $ left show maybeChan
  mightFail <- liftIO $ (try $ AMQP.confirmSelect channel False :: IO (Either AMQP.AMQPException ()))
  except $ left show mightFail
  return $ rabbitPublish channel exchange routingKey

