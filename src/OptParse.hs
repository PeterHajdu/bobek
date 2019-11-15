{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module OptParse(
    optionParser,
    DestinationOpts(..),
    SourceOpts(..),
    Filter(..),
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

data Filter = Filter { filter :: String } deriving (Show, Generic)
instance ParseRecord Filter where parseRecord = parseRecordWithModifiers mods

data Opts = Opts { src :: SourceOpts
                 , dst :: DestinationOpts
                 , mf  :: Maybe Filter
                 }

optionParser :: Parser Opts -- TODO: help is printed 3 times..
optionParser = Opts
  <$> parseRecord
  <*> parseRecord
  <*> (optional $ parseRecord)
