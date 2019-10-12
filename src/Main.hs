module Main(main) where

import Env
import RabbitMqEnv
import Message
import Destination
import Source
import ReceiveId

import qualified Network.AMQP as AMQP(defaultConnectionOpts)


printError :: IO ()
printError = putStrLn "Unable to initialize rabbitmq environment."

createEnv :: ([Message] -> IO PublishResult) -> ((Int -> IO [Either SourceError Message]), ([ReceiveId] -> IO ())) -> Env
createEnv pub (rec, ack) = MkEnv pub rec ack

main :: IO ()
main = do
  maybePublisher <- createRabbitMqDestination AMQP.defaultConnectionOpts "exchange" "routingKey"
  maybeSource <- createRabbitMqSource AMQP.defaultConnectionOpts "queue"
  maybe printError runMover (createEnv <$> maybePublisher <*> maybeSource)
