module Bobek.FileEnv (serializeMessage, writeToFile, createStdoutDestination, createStdinSource, createFileSource, createFileDestination) where

import Bobek.Destination
import Bobek.Env (SourceFunctions (..))
import Bobek.Message
import Bobek.ReceiveId
import Bobek.Source
import Control.Arrow (left)
import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as BSC (ByteString, append, cons, hGetLine, hPutStrLn, span)
import System.IO (hFlush, openFile)
import System.IO.Error (isEOFError)

routingKeySeparator :: Char
routingKeySeparator = ' '

catchIO :: IO a -> IO (Either IOException a)
catchIO = try

parseRoutingKeyAndMessage :: BSC.ByteString -> Either NoMessageReason Message
parseRoutingKeyAndMessage line =
  case BSC.span (/= routingKeySeparator) line of
    (_, "") -> Left $ NMRError "Missing routing key."
    (routingK, msg) -> Right $ MkMessage (MkReceiveId 0) (decodeUtf8 routingK) msg

readFromFile :: Handle -> IO (Either NoMessageReason Message)
readFromFile handle = do
  maybeLine <- catchIO $ BSC.hGetLine handle
  let maybeBody = left mapError maybeLine
  return $ maybeBody >>= parseRoutingKeyAndMessage
  where
    mapError e = if isEOFError e then NMREmptyQueue else NMRError $ show e

createStdinSource :: SourceFunctions
createStdinSource = MkSourceFunctions (readFromFile stdin) (const $ return ())

createFileSource :: FilePath -> IO (Either Text SourceFunctions)
createFileSource filePath = do
  maybeHandle <- catchIO $ openFile filePath ReadMode
  return $ bimap show (\handle -> MkSourceFunctions (readFromFile handle) (const $ return ())) maybeHandle

serializeMessage :: Message -> BSC.ByteString
serializeMessage (MkMessage _ routingK msg) = encodeUtf8 routingK `BSC.append` BSC.cons routingKeySeparator msg

writeToFile :: IO () -> (BSC.ByteString -> IO ()) -> [Message] -> IO PublishResult
writeToFile flush write messages = do
  results <- traverse writeMessage messages
  _ <- flush
  return $ MkPublishResult (lefts results) (rights results)
  where
    writeMessage :: Message -> IO (Either ReceiveId ReceiveId)
    writeMessage msg = do
      result <- catchIO $ write (serializeMessage msg)
      let rid = receiveId msg
      return $ bimap (const rid) (const rid) result

makeWriter :: Handle -> [Message] -> IO PublishResult
makeWriter handle = writeToFile (hFlush handle) (BSC.hPutStrLn handle)

createFileDestination :: FilePath -> IO (Either Text ([Message] -> IO PublishResult))
createFileDestination filePath = do
  maybeHandle <- catchIO $ openFile filePath AppendMode
  return $ bimap show makeWriter maybeHandle

createStdoutDestination :: [Message] -> IO PublishResult
createStdoutDestination = makeWriter stdout
