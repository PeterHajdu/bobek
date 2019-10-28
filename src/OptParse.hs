{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module OptParse(
    -- optionParser,
    Args(..),
    Path(..),
    Uri(..),
    Queue(..),
    Exchange(..),
    RoutingKey(..),
    areArgsValid
) where

import Data.Text
import Options.Generic

newtype Path = Path Text deriving (Generic, Read, Show)
instance ParseRecord Path
instance ParseField Path

newtype Uri = Uri Text deriving (Generic, Read, Show)
instance ParseRecord Uri
instance ParseField Uri

newtype Queue = Queue Text deriving (Generic, Read, Show)
instance ParseRecord Queue
instance ParseField Queue

newtype Exchange = Exchange Text deriving (Generic, Read, Show)
instance ParseRecord Exchange
instance ParseField Exchange

newtype RoutingKey = RoutingKey Text deriving (Generic, Read, Show)
instance ParseRecord RoutingKey
instance ParseField RoutingKey

data Args w = Args { src_file :: w ::: Maybe Path     <?> "Path to source file (instead of queue)."
                   , src_uri :: w ::: Maybe Uri       <?> "URI to RabbitMq of source queue."
                   , queue :: w ::: Maybe Queue       <?> "Name of source queue."
                   ---------------------------------------------------------
                   , dst_file :: w ::: Maybe Path     <?> "Path to destination file (instead of exchange)."
                   , dst_uri :: w ::: Maybe Uri       <?> "URI to RabbitMq of destination exchange."
                   , exchange :: w ::: Maybe Exchange <?> "Name of destination exchange."
                   , key :: w ::: Maybe RoutingKey    <?> "Routing key to use with exchange (optional)."
                   } deriving (Generic)
instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

isSrcValid :: Args Unwrapped -> Bool
isSrcValid (Args (Just _) Nothing Nothing  _ _ _ _) = True
isSrcValid (Args Nothing (Just _) (Just _) _ _ _ _) = True
isSrcValid _ = False

isDestValid :: Args Unwrapped -> Bool
isDestValid (Args _ _ _ (Just _) Nothing Nothing Nothing) = True
isDestValid (Args _ _ _ Nothing (Just _) (Just _) _     ) = True
isDestValid _ = False

areArgsValid :: Args Unwrapped -> Bool
areArgsValid args = isSrcValid args && isDestValid args
