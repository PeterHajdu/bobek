module RabbitMqEnv(createRabbitMqSource, createRabbitMqDestination) where

import Message
import Source
import Destination
import ReceiveId

import Data.Text
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Network.AMQP as AMQP(ConnectionOpts, publishMsg, DeliveryMode(..), newMsg, openChannel, openConnection'', Channel, getMsg, Message(..), Ack(..), ackMsg, Envelope(..), confirmSelect)
import Control.Monad

rabbitPublish :: AMQP.Channel -> Text -> Text -> [Message] -> IO PublishResult
rabbitPublish channel exchange routingkey messages = do
  _ <- traverse (\msg -> AMQP.publishMsg channel exchange routingkey (rabbitMessageFromMessage msg)) messages
  return $ MkPublishResult [] (receiveId <$> messages)

messageFromRabbitMessage :: (AMQP.Message, AMQP.Envelope) -> Message
messageFromRabbitMessage (rabbitMessage, envelope) =
  let rid = MkReceiveId (AMQP.envDeliveryTag envelope)
      body = toStrict $ AMQP.msgBody rabbitMessage
   in MkMessage rid body

rabbitMessageFromMessage :: Message -> AMQP.Message
rabbitMessageFromMessage (MkMessage _ body) =
  AMQP.newMsg {AMQP.msgBody = fromStrict body, AMQP.msgDeliveryMode = Just AMQP.Persistent}

rabbitReceive :: AMQP.Channel -> Text -> Int -> IO [Either SourceError Message]
rabbitReceive channel queue _ = do
  maybeMessage <- AMQP.getMsg channel AMQP.Ack queue
  return $ [maybe (Left $ MkSourceError "kutyus") (Right . messageFromRabbitMessage) maybeMessage]

rabbitAcknowledge :: AMQP.Channel -> [ReceiveId] -> IO ()
rabbitAcknowledge channel ids = void $ traverse (\(MkReceiveId i) -> AMQP.ackMsg channel i False) ids

createRabbitMqSource :: AMQP.ConnectionOpts -> Text -> IO (Maybe (Int -> IO [Either SourceError Message], [ReceiveId] -> IO ()))
createRabbitMqSource opts queue = do
  connection <- AMQP.openConnection'' opts
  channel <- AMQP.openChannel connection
  return $ Just (rabbitReceive channel queue, rabbitAcknowledge channel)

createRabbitMqDestination :: AMQP.ConnectionOpts -> Text -> Text -> IO (Maybe ([Message] -> IO PublishResult))
createRabbitMqDestination opts exchange routingKey = do
  connection <- AMQP.openConnection'' opts
  channel <- AMQP.openChannel connection
  AMQP.confirmSelect channel False
  return $ Just (rabbitPublish channel exchange routingKey)

