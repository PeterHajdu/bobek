module Main(main) where

import Env
import RabbitMqEnv
import FileEnv
import Filter(FilterActions(..), FilterAction(..))
import qualified Message as M
import Destination
import OptParse
import Data.Text
import ScriptFilter

import qualified Network.AMQP as AMQP(fromURI)

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

printError :: String -> IO ()
printError errorMsg = putStrLn $ "Unable to initialize rabbitmq environment: " ++ errorMsg

createEnv :: (M.Message -> IO FilterActions) -> ([M.Message] -> IO PublishResult) -> SourceFunctions -> Env
createEnv fltr pub sourceFuncs = MkEnv pub sourceFuncs fltr

type PublisherFunction = Either String ([M.Message] -> IO PublishResult)

createDestination :: DestinationOpts -> IO PublisherFunction
createDestination Stdout = return $ Right $ createStdoutDestination
createDestination (Outfile filePath) = createFileDestination filePath
createDestination (Exchange uri ex maybeRk) =
    createRabbitMqDestination (AMQP.fromURI uri) (pack ex) (pack <$> maybeRk)

createSource :: SourceOpts -> IO (Either String SourceFunctions)
createSource Stdin = return $ Right $ createStdinSource
createSource (Infile filePath) = createFileSource filePath
createSource (Queue uri queueName) = createRabbitMqSource (AMQP.fromURI uri) (pack queueName)

createFilter :: FilterOpt -> (M.Message -> IO FilterActions)
createFilter DontAck = const $ return $ MkFilterActions [Copy]
createFilter (ScriptFilter scriptPath) = scriptFilter scriptPath

main :: IO ()
main = do
  opts <- execParser $ info optionParser
    $ header "RabbitMq Swiss Army Knife" <>
      footer "This utility moves messages from a source to a destination."
  maybePublisher <- createDestination $ dst opts
  maybeSource <- createSource $ src opts
  let mfilter = fromMaybe defaultFilter (createFilter <$> (messageFilter opts))
  either printError runMover ((createEnv mfilter) <$> maybePublisher <*> maybeSource)
