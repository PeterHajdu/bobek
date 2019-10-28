{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Env
import RabbitMqEnv
import FileEnv
import qualified Message as M
import Destination
import Source
import ReceiveId
import OptParse
import Data.Text

import qualified Network.AMQP as AMQP(fromURI)

import Options.Generic

printError :: String -> IO ()
printError errorMsg = putStrLn $ "Unable to initialize rabbitmq environment: " ++ errorMsg

createEnv :: ([M.Message] -> IO PublishResult) -> ((IO (Either NoMessageReason M.Message)), ([ReceiveId] -> IO ())) -> Env
createEnv pub (rec, ack) = MkEnv pub rec ack

type PublisherFunction = Either String ([M.Message] -> IO PublishResult)
type SourceFunctions = Either String (IO (Either NoMessageReason M.Message), [ReceiveId] -> IO ())

createDestination :: Maybe Path -> Maybe Uri -> Maybe Exchange -> Maybe RoutingKey -> IO PublisherFunction
createDestination (Just (Path path)) _                _                          _         = createFileDestination $ unpack path
createDestination _                  (Just (Uri uri)) (Just (Exchange ex)) maybeRoutingKey =
  createRabbitMqDestination
    (AMQP.fromURI $ unpack uri)
    ex
    ((\(RoutingKey rk) -> rk) <$> maybeRoutingKey)
createDestination _ _ _ _ = pure $ Left "Please provide both RabbitMQ URI and exchange name for destination."

createSource :: Maybe Path -> Maybe Uri -> Maybe Queue -> IO SourceFunctions
createSource (Just (Path path)) _                _                = createFileSource $ unpack path
createSource _                  (Just (Uri uri)) (Just (Queue q)) = createRabbitMqSource (AMQP.fromURI (unpack uri)) q
createSource _ _ _ = pure $ Left "Please provide both RabbitMQ URI and queue name for source."

bootstrap :: Args Unwrapped -> IO ()
bootstrap (Args srcFile srcUri srcQueue dstFile destUri dstExchange routingKey) = do
  maybePublisher <- createDestination dstFile destUri dstExchange routingKey
  maybeSource <- createSource srcFile srcUri srcQueue
  either printError runMover (createEnv <$> maybePublisher <*> maybeSource)

main :: IO ()
main = do
  (args, printHelp) <- unwrapWithHelp "Rabbitmq Swiss Army Knife" :: IO (Args Unwrapped, IO ())
  if (areArgsValid args)
    then bootstrap args
    else
      let msg = "Necessary args might be missing or were invalid (only 1 type of source and 1 type of destination is allowed)."
      in
        putStrLn msg >> printHelp
