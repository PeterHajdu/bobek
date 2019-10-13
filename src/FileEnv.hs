module FileEnv(createFileSource, createFileDestination) where

import Message
import Source
import Destination
import ReceiveId

import qualified Data.ByteString.Char8 as BS(getLine, hPutStrLn)

import System.IO

recv :: Int -> IO [Either SourceError Message]
recv _ = do
  body <- BS.getLine
  return $ [Right $ MkMessage (MkReceiveId 0) body]

createFileSource :: IO (Maybe (Int -> IO [Either SourceError Message], [ReceiveId] -> IO ()))
createFileSource = return $ Just (recv, (const $ return ()))

writeToFile :: Handle -> [Message] -> IO PublishResult
writeToFile handle messages = do
  let messageBodies = message <$> messages
  let messageIds = receiveId <$> messages
  _ <- traverse (BS.hPutStrLn handle) messageBodies
  hFlush handle
  return $ MkPublishResult [] messageIds

createFileDestination :: FilePath -> IO (Maybe ([Message] -> IO PublishResult))
createFileDestination filePath = do
  handle <- openFile filePath AppendMode
  return $ Just $ writeToFile handle
