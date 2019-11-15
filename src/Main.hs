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
import Data.Function ((&))

printError :: String -> IO ()
printError errorMsg = putStrLn $ "Unable to initialize rabbitmq environment: " ++ errorMsg

createEnv :: (M.Message -> IO FilterAction) -> ([M.Message] -> IO PublishResult) -> ((IO (Either NoMessageReason M.Message)), ([ReceiveId] -> IO ())) -> Env
createEnv fltr pub (rec, ack) = MkEnv pub rec ack fltr

type PublisherFunction = Either String ([M.Message] -> IO PublishResult)

createDestination :: DestinationOpts -> IO PublisherFunction
createDestination Stdout = return $ Right $ createStdoutDestination
createDestination (Outfile filePath) = createFileDestination filePath
createDestination (Exchange uri ex maybeRk) =
    createRabbitMqDestination (AMQP.fromURI uri) (pack ex) (pack <$> maybeRk)

type SourceFunctions = Either String (IO (Either NoMessageReason M.Message), [ReceiveId] -> IO ())

createSource :: SourceOpts -> IO SourceFunctions
createSource Stdin = return $ Right $ createStdinSource
createSource (Infile filePath) = createFileSource filePath
createSource (Queue uri queueName) = createRabbitMqSource (AMQP.fromURI uri) (pack queueName)

main :: IO ()
main = do
  opts <- execParser $ info optionParser
    $ header "RabbitMq Swiss Army Knife" <>
      footer "This utility moves messages from a source to a destination."
  maybePublisher <- createDestination $ dst opts
  maybeSource <- createSource $ src opts
  let filterPath = opts & mf & fmap OptParse.filter
  let filter' = maybe defaultFilter scriptFilter filterPath
  either printError runMover ((createEnv filter') <$> maybePublisher <*> maybeSource)
