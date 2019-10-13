module Main(main) where

import Env
import RabbitMqEnv
import Message
import Destination
import Source
import ReceiveId

import Data.Text
import qualified Network.AMQP as AMQP(defaultConnectionOpts)

import Options.Applicative
import Data.Semigroup ((<>))

data Opts = MkOpts
  { exchange :: Text
  , routingKey :: Text
  , sourceQueue :: Text
  }

optionParser :: Parser Opts
optionParser = MkOpts
      <$> strOption
          ( long "exchange-name"
         <> metavar "TARGET"
         <> short 'e'
         <> help "destination exchange" )
      <*> strOption
          ( long "routing-key"
         <> short 'r'
         <> help "routing key to send messages with" )
      <*> strOption
          ( long "queue"
         <> short 'q'
         <> help "source queue")

printError :: String -> IO ()
printError errorMsg = putStrLn $ "Unable to initialize rabbitmq environment: " ++ errorMsg

createEnv :: ([Message] -> IO PublishResult) -> ((IO (Either SourceError Message)), ([ReceiveId] -> IO ())) -> Env
createEnv pub (rec, ack) = MkEnv pub rec ack

main :: IO ()
main = do
  opts <- execParser (info optionParser (fullDesc <> progDesc "rabbitmq swiss army knife"))
  maybePublisher <- createRabbitMqDestination AMQP.defaultConnectionOpts (exchange opts) (routingKey opts)
  maybeSource <- createRabbitMqSource AMQP.defaultConnectionOpts (sourceQueue opts)
  either printError runMover (createEnv <$> maybePublisher <*> maybeSource)
