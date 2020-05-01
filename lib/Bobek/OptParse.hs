module Bobek.OptParse
  ( runArgParser,
    DestinationOpts (..),
    SourceOpts (..),
    FilterOpts (..),
    Opts (..),
    Uri (..),
    Path (..),
    RoutingKey (..),
  )
where

import Data.Text
import Options.Applicative

newtype Uri = Uri {unUri :: Text} deriving stock (Show)

newtype Path = Path {unPath :: Text} deriving stock (Show)

newtype RoutingKey = RoutingKey {unKey :: Text} deriving stock (Show)

data SourceOpts
  = Stdin
  | Infile Path
  | Queue Uri Text
  deriving stock (Show)

stdin :: Parser SourceOpts
stdin =
  flag' Stdin $
    long "stdin"
      <> help "Read from standard input"

infile :: Parser SourceOpts
infile =
  Infile . Path
    <$> strOption
      ( long "infile"
          <> short 'i'
          <> metavar "INPUT_FILE"
          <> help "Path to input file"
      )

queue :: Parser SourceOpts
queue =
  Queue . Uri
    <$> strOption
      ( long "srcuri"
          <> metavar "SRC_AMQP_URI"
          <> short 's'
          <> help "URI of source RabbitMq vhost"
      )
    <*> strOption
      ( long "queue"
          <> metavar "QUEUE_NAME"
          <> short 'q'
          <> help "Name of source queue"
      )

srcParser :: Parser SourceOpts
srcParser = stdin <|> infile <|> queue

data DestinationOpts
  = Stdout
  | Outfile Path
  | Exchange Uri Text (Maybe RoutingKey)
  deriving stock (Show)

stdout :: Parser DestinationOpts
stdout =
  flag' Stdout $
    long "stdout"
      <> help "Print to standard output"

outfile :: Parser DestinationOpts
outfile =
  Outfile . Path
    <$> strOption
      ( long "outfile"
          <> short 'o'
          <> metavar "OUTPUT_FILE"
          <> help "Path to output file"
      )

exchange :: Parser DestinationOpts
exchange =
  Exchange . Uri
    <$> strOption
      ( long "dsturi"
          <> metavar "DST_AMQP_URI"
          <> short 'd'
          <> help "URI of destination RabbitMq vhost"
      )
    <*> strOption
      ( long "exchange"
          <> metavar "EXCHANGE"
          <> short 'e'
          <> help "Name of destination exchange"
      )
    <*> optional
      ( RoutingKey
          <$> strOption
            ( long "key"
                <> metavar "ROUTING_KEY"
                <> short 'k'
                <> help "Routing key of your messages [optional]"
            )
      )

dstParser :: Parser DestinationOpts
dstParser = stdout <|> outfile <|> exchange

data FilterOpts
  = DontAck
  | FilterScript Text
  deriving stock (Show)

dontack :: Parser FilterOpts
dontack =
  flag' DontAck $
    long "dontack"
      <> help "Do not wait for acknowledgment"

fscript :: Parser FilterOpts
fscript =
  FilterScript
    <$> strOption
      ( long "filter"
          <> short 'f'
          <> metavar "SCRIPT_PATH"
          <> help "Path to filter script [optional]"
      )

filterOptParser :: Parser FilterOpts
filterOptParser = dontack <|> fscript

data Opts = Opts
  { src :: SourceOpts,
    dst :: DestinationOpts,
    messageFilter :: Maybe FilterOpts,
    debug :: Bool
  }

optionParser :: Parser Opts
optionParser =
  Opts
    <$> srcParser
    <*> dstParser
    <*> optional filterOptParser
    <*> switch (long "debug" <> help "Enable debug logging")

runArgParser :: IO Opts
runArgParser = customExecParser p opts
  where
    things =
      header "bobek - RabbitMq Swiss Army Knife"
        <> progDesc "This utility moves messages from a source to a destination."
    opts = info (optionParser <**> helper) things
    p = prefs $ showHelpOnEmpty <> showHelpOnError
