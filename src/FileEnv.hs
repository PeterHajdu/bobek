module FileEnv(createFileSource, createFileDestination) where

import Message
import Source
import Destination
import ReceiveId

import qualified Data.ByteString.Char8 as BSC(append, cons, hGetLine, hPutStrLn, ByteString, span)

import Data.Text.Encoding(encodeUtf8, decodeUtf8)

import System.IO(Handle, IOMode(ReadMode, AppendMode), openFile, hFlush)
import System.IO.Error(isEOFError )

import Control.Exception(IOException, try)
import Data.Bifunctor(bimap)
import Data.Either(lefts, rights)
import Control.Arrow(left)

import Data.Text()

routingKeySeparator :: Char
routingKeySeparator = ' '

parseRoutingKeyAndMessage :: BSC.ByteString -> Either NoMessageReason Message
parseRoutingKeyAndMessage line =
  case BSC.span (/= routingKeySeparator) line of
    (_, "") -> Left $ NMRError "Missing routing key."
    (routingK, msg) -> Right $ MkMessage (MkReceiveId 0) (decodeUtf8 routingK) msg

recv :: Handle -> IO (Either NoMessageReason Message)
recv handle = do
  maybeBody <- (left mapError) <$> (try $ BSC.hGetLine handle) :: IO (Either NoMessageReason BSC.ByteString)
  return $ maybeBody >>= parseRoutingKeyAndMessage
  where mapError e = if isEOFError e then NMREmptyQueue else NMRError $ show e

createFileSource :: FilePath -> IO (Either String (IO (Either NoMessageReason Message), [ReceiveId] -> IO ()))
createFileSource filePath = do
  maybeHandle <- try $ openFile filePath ReadMode :: IO (Either IOException Handle)
  return $ bimap show (\handle -> (recv handle, const $ return ())) maybeHandle

serializeMessage :: Message -> BSC.ByteString
serializeMessage (MkMessage _ routingK msg) = (encodeUtf8 routingK) `BSC.append` (BSC.cons routingKeySeparator msg)

writeToFile :: Handle -> [Message] -> IO PublishResult
writeToFile handle messages = do
  results <- traverse writeMessage messages
  hFlush handle
  return $ MkPublishResult (lefts results) (rights results)
  where writeMessage :: Message -> IO (Either ReceiveId ReceiveId)
        writeMessage msg = do
          result <- try $ BSC.hPutStrLn handle (serializeMessage msg) :: IO (Either IOException ())
          let rid = receiveId msg
          return $ bimap (const rid) (const rid) result

createFileDestination :: FilePath -> IO (Either String ([Message] -> IO PublishResult))
createFileDestination filePath = do
  maybeHandle <- try $ openFile filePath AppendMode :: IO (Either IOException Handle)
  return $ bimap show writeToFile maybeHandle
