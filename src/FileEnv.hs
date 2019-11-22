module FileEnv(createStdoutDestination, createStdinSource, createFileSource, createFileDestination) where

import Message
import Source
import Destination
import ReceiveId
import Util(tshow)

import qualified Data.ByteString.Char8 as BSC(append, cons, hGetLine, hPutStrLn, ByteString, span)

import Data.Text.Encoding(encodeUtf8, decodeUtf8)

import System.IO(Handle, IOMode(ReadMode, AppendMode), openFile, hFlush, stdout, stdin)
import System.IO.Error(isEOFError )

import Control.Exception(IOException, try)
import Data.Bifunctor(bimap)
import Data.Either(lefts, rights)
import Control.Arrow(left)

import qualified Data.Text as T

import Env(SourceFunctions(..))

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
  where mapError e = if isEOFError e then NMREmptyQueue else NMRError $ show e

createStdinSource :: SourceFunctions
createStdinSource = MkSourceFunctions (readFromFile stdin) (const $ return ())

createFileSource :: FilePath -> IO (Either T.Text SourceFunctions)
createFileSource filePath = do
  maybeHandle <- catchIO $ openFile filePath ReadMode
  return $ bimap tshow (\handle -> MkSourceFunctions (readFromFile handle) (const $ return ())) maybeHandle

serializeMessage :: Message -> BSC.ByteString
serializeMessage (MkMessage _ routingK msg) = (encodeUtf8 routingK) `BSC.append` (BSC.cons routingKeySeparator msg)

writeToFile :: Handle -> [Message] -> IO PublishResult
writeToFile handle messages = do
  results <- traverse writeMessage messages
  hFlush handle
  return $ MkPublishResult (lefts results) (rights results)
  where writeMessage :: Message -> IO (Either ReceiveId ReceiveId)
        writeMessage msg = do
          result <- catchIO $ BSC.hPutStrLn handle (serializeMessage msg)
          let rid = receiveId msg
          return $ bimap (const rid) (const rid) result

createFileDestination :: FilePath -> IO (Either T.Text ([Message] -> IO PublishResult))
createFileDestination filePath = do
  maybeHandle <- catchIO $ openFile filePath AppendMode
  return $ bimap tshow writeToFile maybeHandle

createStdoutDestination :: [Message] -> IO PublishResult
createStdoutDestination = writeToFile stdout
