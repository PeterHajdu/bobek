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

printError :: IO ()
printError = putStrLn "Unable to initialize rabbitmq environment."

createEnv :: ([Message] -> IO PublishResult) -> ((Int -> IO [Either SourceError Message]), ([ReceiveId] -> IO ())) -> Env
createEnv pub (rec, ack) = MkEnv pub rec ack

main :: IO ()
main = do
  opts <- execParser (info optionParser (fullDesc <> progDesc "rabbitmq swiss army knife"))
  maybePublisher <- createRabbitMqDestination AMQP.defaultConnectionOpts (exchange opts) (routingKey opts)
  maybeSource <- createRabbitMqSource AMQP.defaultConnectionOpts (sourceQueue opts)
  maybe printError runMover (createEnv <$> maybePublisher <*> maybeSource)
