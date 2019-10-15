module FileEnv(createFileSource, createFileDestination) where

import Message
import Source
import Destination
import ReceiveId

import qualified Data.ByteString.Char8 as BS(hGetLine, hPutStrLn, ByteString)

import System.IO(Handle, IOMode(ReadMode, AppendMode), openFile, hFlush)
import System.IO.Error(isEOFError )

import Control.Exception(IOException, try)
import Data.Bifunctor(bimap)
import Data.Either(lefts, rights)

recv :: Handle -> IO (Either NoMessageReason Message)
recv handle = do
  maybeBody <- try $ BS.hGetLine handle :: IO (Either IOException BS.ByteString)
  return $ bimap mapError (MkMessage (MkReceiveId 0)) maybeBody
  where mapError e = if isEOFError e then NMREmptyQueue else NMRError $ show e

createFileSource :: FilePath -> IO (Either String (IO (Either NoMessageReason Message), [ReceiveId] -> IO ()))
createFileSource filePath = do
  maybeHandle <- try $ openFile filePath ReadMode :: IO (Either IOException Handle)
  return $ bimap show (\handle -> (recv handle, const $ return ())) maybeHandle

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
