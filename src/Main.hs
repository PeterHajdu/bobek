module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text
import Destination
import Env
import FileEnv
import Filter (FilterAction (..), FilterActions (..))
import Log
import qualified Message as M
import qualified Network.AMQP as AMQP (fromURI)
import OptParse
import Options.Applicative
import RabbitMqEnv
import ScriptFilter

printError :: Text -> IO ()
printError errorMsg = ioErrorLog $ "Unable to initialize rabbitmq environment: " `append` errorMsg

type PublisherFunction = Either Text ([M.Message] -> IO PublishResult)

createDestination :: DestinationOpts -> IO PublisherFunction
createDestination Stdout = return $ Right $ createStdoutDestination
createDestination (Outfile filePath) = createFileDestination filePath
createDestination (Exchange uri ex maybeRk) =
  createRabbitMqDestination (AMQP.fromURI uri) (pack ex) (pack <$> maybeRk)

createSource :: SourceOpts -> IO (Either Text SourceFunctions)
createSource Stdin = return $ Right $ createStdinSource
createSource (Infile filePath) = createFileSource filePath
createSource (Queue uri queueName) = createRabbitMqSource (AMQP.fromURI uri) (pack queueName)

createFilter :: FilterOpt -> (M.Message -> IO FilterActions)
createFilter DontAck = const $ return $ MkFilterActions [Copy]
createFilter (ScriptFilter scriptPath) = scriptFilter scriptPath

main :: IO ()
main = do
  opts <-
    execParser $ info optionParser $
      header "RabbitMq Swiss Army Knife"
        <> footer "This utility moves messages from a source to a destination."
  maybePublisher <- createDestination $ dst opts
  maybeSource <- createSource $ src opts
  let mfilter = fromMaybe defaultFilter (createFilter <$> (messageFilter opts))
  let logfunctions = if (debug opts) then logWithDebug else logOnlyErrors
  either printError runMover (MkEnv <$> maybePublisher <*> maybeSource <*> (Right mfilter) <*> (Right logfunctions))
