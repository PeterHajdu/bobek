{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OptParse(optionParser) where

import Options.Applicative
import Data.Text
import Data.String(IsString)

newtype URI = MkUri Text deriving(Data.String.IsString, Show)
newtype Path = MkPath Text deriving(Data.String.IsString, Show)

data Opts = MkOpts
    { source :: Source
    , destination :: Destination
    } deriving(Show)

data Source
    = AmqpSource URI Text
    | SrcFile Path
    deriving(Show)

data Destination
    = AmqpDestination URI Text
    | DestFile Path
    deriving(Show)

optionParser :: Parser Opts
optionParser = MkOpts
    <$> (srcAmqpOpt <|> srcFileOpt)
    <*> (destAmqpOpt <|> destFileOpt)

srcAmqpOpt :: Parser Source
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

srcFileOpt :: Parser Source
srcFileOpt = SrcFile
    <$> strOption
        (   long "input-file"
        <> short 'i'
        <> metavar "FILE"
        <>  help "Source/input file path" )

destAmqpOpt :: Parser Destination
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

destFileOpt :: Parser Destination
destFileOpt = DestFile
    <$> strOption
        ( long "destination-file"
        <> short 'o'
        <> metavar "DESTFILE"
        <> help "Destination/output file path" )
