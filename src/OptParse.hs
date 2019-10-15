{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OptParse(
    optionParser,
    Opts(..),
    DestinationOpts(..),
    Path(..),
    URI(..)
) where

import Options.Applicative
import Data.Text
import Data.String(IsString)
import Control.Applicative

newtype URI = MkUri Text deriving(Data.String.IsString, Show)
newtype Path = MkPath Text deriving(Data.String.IsString, Show)

data Opts = MkOpts
    { source :: SourceOpts
    , destination :: DestinationOpts
    } deriving(Show)

data SourceOpts
    = AmqpSource URI Text
    | SrcFile Path
    deriving(Show)

data DestinationOpts
    = AmqpDestination URI Text (Maybe Text) -- connection-uri, exchange, maybe routingkey
    | DestFile Path
    deriving(Show)

optionParser :: Parser Opts
optionParser = MkOpts
    <$> (srcAmqpOpt <|> srcFileOpt)
    <*> (destAmqpOpt <|> destFileOpt)

srcAmqpOpt :: Parser SourceOpts
srcAmqpOpt = AmqpSource
    <$> strOption
        (   long "source-amqp"
        <>  short 's'
        <>  metavar "SRC_AMQP"
        <>  help "Source AMQP URI. Example: TODO"
        )
    <*> strOption
        (   long "source-queue"
        <> short 'q'
        <>  metavar "SRC_QUEUE"
        <>  help "Source queue."
        )

srcFileOpt :: Parser SourceOpts
srcFileOpt = SrcFile
    <$> strOption
        (   long "input-file"
        <> short 'i'
        <> metavar "FILE"
        <>  help "Source/input file path" )

destAmqpOpt :: Parser DestinationOpts
destAmqpOpt = AmqpDestination
    <$> strOption
        (  long "destination-amqp"
        <> short 'd'
        <> metavar "DEST_AMQP"
        <> help "Destination AMQP URI. Example: TODO"
        )
    <*> strOption
        ( long "exchange"
        <> short 'e'
        <> metavar "DEST_EXCHANGE"
        <> help "Destination exchange" )
    <*> ( optional $ strOption
            ( long "routing-key"
            <> short 'r'
            <> metavar "DEST_ROUTING_KEY"
            <> help "Override routing key for publishing" )
        )

destFileOpt :: Parser DestinationOpts
destFileOpt = DestFile
    <$> strOption
        ( long "destination-file"
        <> short 'o'
        <> metavar "DESTFILE"
        <> help "Destination/output file path" )
