module Main (main) where

import Bobek.Destination
import Bobek.Env
import Bobek.FileEnv
import Bobek.Filter (FilterAction (..), FilterActions (..))
import Bobek.Log
import qualified Bobek.Message as M
import Bobek.OptParse
import Bobek.RabbitMqEnv
import Bobek.ScriptFilter
import Data.Maybe (maybe)
import Data.Semigroup ()
import Data.Text
import qualified Network.AMQP as AMQP (fromURI)

printError :: Text -> IO ()
printError errorMsg = ioErrorLog $ "Unable to initialize rabbitmq environment: " `append` errorMsg

type PublisherFunction = Either Text ([M.Message] -> IO PublishResult)

createDestination :: DestinationOpts -> IO PublisherFunction
createDestination Stdout = return $ Right $ createStdoutDestination
createDestination (Outfile (Path filePath)) = createFileDestination . unpack $ filePath
createDestination (Exchange (Uri uri) ex maybeRk) =
  createRabbitMqDestination (AMQP.fromURI . unpack $ uri) ex (unKey <$> maybeRk)

createSource :: SourceOpts -> IO (Either Text SourceFunctions)
createSource Stdin = return $ Right $ createStdinSource
createSource (Infile (Path filePath)) = createFileSource $ unpack filePath
createSource (Queue (Uri uri) queueName) = createRabbitMqSource (AMQP.fromURI . unpack $ uri) queueName

createFilter :: FilterOpts -> (M.Message -> IO FilterActions)
createFilter DontAck = const $ return $ MkFilterActions [Copy]
createFilter (FilterScript scriptPath) = scriptFilter $ unpack scriptPath

main :: IO ()
main = do
  opts <- runArgParser
  maybePublisher <- createDestination $ dst opts
  maybeSource <- createSource $ src opts
  let mfilter = maybe defaultFilter createFilter (messageFilter opts)
  let logfunctions = if debug opts then logWithDebug else logOnlyErrors
  either printError runMover $ MkEnv <$> maybePublisher <*> maybeSource <*> Right mfilter <*> Right logfunctions
