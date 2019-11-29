{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module OptParse(
    optionParser,
    DestinationOpts(..),
    SourceOpts(..),
    FilterOpt(..),
    Opts(..)
) where

import Options.Generic
import Options.Applicative
import Data.Char

mods :: Modifiers
mods = defaultModifiers { fieldNameModifier = map toLower
                        , shortNameModifier = firstLetter
                        }

data SourceOpts = Stdin
                | Infile String
                | Queue { srcUri :: String
                        , queue  :: String
                        } deriving (Show, Generic)
instance ParseRecord SourceOpts  where parseRecord = parseRecordWithModifiers mods

data DestinationOpts = Stdout
                     | Outfile String
                     | Exchange { dstUri :: String
                                , exchange :: String
                                , routingKey :: Maybe String
                                } deriving (Show, Generic)
instance ParseRecord DestinationOpts where parseRecord = parseRecordWithModifiers mods

data FilterOpt = DontAck | ScriptFilter String deriving (Show, Generic)

instance ParseRecord FilterOpt where parseRecord = parseRecordWithModifiers mods

data Opts = Opts { src :: SourceOpts
                 , dst :: DestinationOpts
                 , messageFilter :: Maybe FilterOpt
                 , debug :: Bool
                 }

optionParser :: Parser Opts -- TODO: help is printed 3 times..
optionParser = Opts
  <$> parseRecord
  <*> parseRecord
  <*> (optional $ parseRecord)
  <*> switch(long "debug" <> help "Enable debug logging" )
