module FileEnv(createFileSource, createFileDestination) where

import Message
import Source
import Destination
import ReceiveId

import Data.ByteString()
import Data.ByteString.Char8 (pack)

recv :: Int -> IO [Either SourceError Message]
recv _ = do
  body <- (pack <$> getLine)
  return $ [Right $ MkMessage (MkReceiveId 0) body]

createFileSource :: IO (Maybe (Int -> IO [Either SourceError Message], [ReceiveId] -> IO ()))
createFileSource = return $ Just (recv, (const $ return ()))

createFileDestination :: IO (Maybe ([Message] -> IO PublishResult))
createFileDestination = return $ Just undefined
