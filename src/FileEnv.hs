module FileEnv(createFileSource, createFileDestination) where

import Message
import Source
import Destination
import ReceiveId

import qualified Data.ByteString.Char8 as BS(getLine, hPutStrLn, ByteString)

import System.IO(Handle, IOMode(AppendMode), openFile, hFlush)

import Control.Exception(IOException, try)
import Data.Bifunctor(bimap)
import Data.Either(lefts, rights)

recv :: IO (Either SourceError Message)
recv = do
  maybeBody <- try $ BS.getLine :: IO (Either IOException BS.ByteString)
  return $ bimap (MkSourceError . show) (MkMessage (MkReceiveId 0)) maybeBody

createFileSource :: IO (Maybe (IO (Either SourceError Message), [ReceiveId] -> IO ()))
createFileSource = return $ Just (recv, (const $ return ()))

writeToFile :: Handle -> [Message] -> IO PublishResult
writeToFile handle messages = do
  results <- traverse writeMessage messages
  hFlush handle
  return $ MkPublishResult (lefts results) (rights results)
  where writeMessage :: Message -> IO (Either ReceiveId ReceiveId)
        writeMessage msg = do
          result <- try $ BS.hPutStrLn handle (message msg) :: IO (Either IOException ())
          let rid = receiveId msg
          return $ bimap (const rid) (const rid) result

createFileDestination :: FilePath -> IO (Either String ([Message] -> IO PublishResult))
createFileDestination filePath = do
  maybeHandle <- try $ openFile filePath AppendMode :: IO (Either IOException Handle)
  return $ bimap show writeToFile maybeHandle
