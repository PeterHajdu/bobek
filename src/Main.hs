module Main(main) where

import Env
import RabbitMqEnv
import Message
import Destination
import Source
import ReceiveId
import OptParse

import Data.Text
import qualified Network.AMQP as AMQP(defaultConnectionOpts)

import Options.Applicative
import Data.Semigroup ((<>))

printError :: String -> IO ()
printError errorMsg = putStrLn $ "Unable to initialize rabbitmq environment: " ++ errorMsg

createEnv :: ([Message] -> IO PublishResult) -> ((IO (Either SourceError Message)), ([ReceiveId] -> IO ())) -> Env
createEnv pub (rec, ack) = MkEnv pub rec ack

createDestination

main :: IO ()
main = do
  opts <- execParser (info optionParser (fullDesc <> progDesc "rabbitmq swiss army knife"))
  maybePublisher <- createRabbitMqDestination AMQP.defaultConnectionOpts ("exchange opts") ("routingKey opts")
  maybeSource <- createRabbitMqSource AMQP.defaultConnectionOpts ("sourceQueue opts")
  either printError runMover (createEnv <$> maybePublisher <*> maybeSource)
