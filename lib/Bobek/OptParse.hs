{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Bobek.OptParse
  ( optionParser,
    DestinationOpts (..),
    SourceOpts (..),
    FilterOpt (..),
    Opts (..),
  )
where

import Data.Char
import Options.Applicative
import Options.Generic

mods :: Modifiers
mods =
  defaultModifiers
    { fieldNameModifier = map toLower,
      shortNameModifier = firstLetter
    }

data SourceOpts
  = Stdin
  | Infile String
  | Queue
      { srcUri :: String,
        queue :: String
      }
  deriving stock (Show, Generic)

instance ParseRecord SourceOpts where parseRecord = parseRecordWithModifiers mods

data DestinationOpts
  = Stdout
  | Outfile String
  | Exchange
      { dstUri :: String,
        exchange :: String,
        routingKey :: Maybe String
      }
  deriving stock (Show, Generic)

instance ParseRecord DestinationOpts where parseRecord = parseRecordWithModifiers mods

data FilterOpt = DontAck | ScriptFilter String deriving stock (Show, Generic)

instance ParseRecord FilterOpt where parseRecord = parseRecordWithModifiers mods

data Opts = Opts
  { src :: SourceOpts,
    dst :: DestinationOpts,
    messageFilter :: Maybe FilterOpt,
    debug :: Bool
  }

optionParser :: Parser Opts -- TODO: help is printed 3 times..
optionParser =
  Opts
    <$> parseRecord
    <*> parseRecord
    <*> (optional $ parseRecord)
    <*> switch (long "debug" <> help "Enable debug logging")
