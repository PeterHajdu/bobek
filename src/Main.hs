module Main(main) where

import Env
import RabbitMqEnv
import FileEnv
import Filter(FilterAction)
import qualified Message as M
import Destination
import Source
import ReceiveId
import OptParse
import Data.Text
import ScriptFilter

import qualified Network.AMQP as AMQP(fromURI)

import Options.Applicative
import Data.Semigroup ((<>))

printError :: String -> IO ()
printError errorMsg = putStrLn $ "Unable to initialize rabbitmq environment: " ++ errorMsg

createEnv :: (M.Message -> IO FilterAction) -> ([M.Message] -> IO PublishResult) -> ((IO (Either NoMessageReason M.Message)), ([ReceiveId] -> IO ())) -> Env
createEnv fltr pub (rec, ack) = MkEnv pub rec ack fltr

type PublisherFunction = Either String ([M.Message] -> IO PublishResult)
type SourceFunctions = Either String (IO (Either NoMessageReason M.Message), [ReceiveId] -> IO ())

createDestination :: DestinationOpts -> IO PublisherFunction
createDestination (DestFile (MkPath path)) = createFileDestination (unpack path)
createDestination (AmqpDestination (MkUri uri) exchange routingKey) =
    createRabbitMqDestination (AMQP.fromURI $ unpack uri) exchange routingKey

createSource :: SourceOpts -> IO SourceFunctions
createSource (SrcFile (MkPath path)) = createFileSource (unpack path)
createSource (AmqpSource (MkUri uri) queue) = createRabbitMqSource (AMQP.fromURI $ unpack uri) queue


main :: IO ()
main = do
  opts <- execParser (info optionParser (fullDesc <> progDesc "rabbitmq swiss army knife"))
  maybePublisher <- createDestination (destination opts)
  maybeSource <- createSource (source opts)
  let fltr = maybe defaultFilter scriptFilter (OptParse.filter opts)
  either printError runMover ((createEnv fltr) <$> maybePublisher <*> maybeSource)
