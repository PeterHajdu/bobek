module Main(main) where

import Env
import RabbitMqEnv
import FileEnv
import Message
import Destination
import Source
import ReceiveId
import OptParse
import Data.Text

import qualified Network.AMQP as AMQP(fromURI)

import Options.Applicative
import Data.Semigroup ((<>))

printError :: String -> IO ()
printError errorMsg = putStrLn $ "Unable to initialize rabbitmq environment: " ++ errorMsg

createEnv :: ([Message] -> IO PublishResult) -> ((IO (Either SourceError Message)), ([ReceiveId] -> IO ())) -> Env
createEnv pub (rec, ack) = MkEnv pub rec ack

type PublisherFunction = Either String ([Message] -> IO PublishResult)
type SourceFunctions = Either String (IO (Either SourceError Message), [ReceiveId] -> IO ())

createDestination :: DestinationOpts -> IO PublisherFunction
createDestination (DestFile (MkPath path)) = createFileDestination (unpack path)
createDestination (AmqpDestination (MkUri uri) exchange (Just routingKey)) =
    createRabbitMqDestination (AMQP.fromURI $ unpack uri) (exchange) (routingKey)
-- TODO: case for routingKey=Nothing

createSource :: SourceOpts -> IO SourceFunctions
createSource (SrcFile (MkPath path)) = createFileSource (unpack path)
createSource (AmqpSource (MkUri uri) queue) = createRabbitMqSource (AMQP.fromURI $ unpack uri) queue


main :: IO ()
main = do
  opts <- execParser (info optionParser (fullDesc <> progDesc "rabbitmq swiss army knife"))
  maybePublisher <- createDestination (destination opts)
  maybeSource <- createSource (source opts)
  either printError runMover (createEnv <$> maybePublisher <*> maybeSource)
