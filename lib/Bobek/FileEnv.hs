module Bobek.FileEnv
  ( readFromFile,
    serializeMessage,
    writeToFile,
    stdinAsSource,
    fileAsSource,
    stdoutAsDestination,
    fileAsDestination,
  )
where

import Bobek.Destination
import Bobek.Message
import Bobek.ReceiveId
import Bobek.Source
import Control.Arrow (left)
import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as BSC (ByteString, append, cons, dropWhile, hGetLine, hPutStrLn, span)
import Polysemy
import Polysemy.Error (Error (..), fromEither, fromEitherM, fromExceptionVia, throw)
import qualified Polysemy.Reader as P
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
    (routingK, msg) -> Right $ MkMessage (MkReceiveId 0) (decodeUtf8 routingK) (BSC.dropWhile (== routingKeySeparator) msg)

readFromFile :: IO BSC.ByteString -> IO (Either NoMessageReason Message)
readFromFile read = do
  maybeLine <- catchIO read
  let maybeBody = left mapError maybeLine
  return $ maybeBody >>= parseRoutingKeyAndMessage
  where
    mapError e = if isEOFError e then NMREmptyQueue else NMRError $ show e

makeReader :: Handle -> IO (Either NoMessageReason Message)
makeReader = readFromFile . BSC.hGetLine

serializeMessage :: Message -> BSC.ByteString
serializeMessage (MkMessage _ routingK msg) = encodeUtf8 routingK `BSC.append` BSC.cons routingKeySeparator msg

writeToFile :: IO () -> (BSC.ByteString -> IO ()) -> [Message] -> IO PublishResult
writeToFile flush write messages = do
  results <- traverse writeMessage messages
  _ <- catchIO flush
  return $ MkPublishResult (lefts results) (rights results)
  where
    writeMessage :: Message -> IO (Either ReceiveId ReceiveId)
    writeMessage msg = do
      result <- catchIO $ write (serializeMessage msg)
      let rid = receiveId msg
      return $ bimap (const rid) (const rid) result

makeWriter :: Handle -> [Message] -> IO PublishResult
makeWriter handle = writeToFile (hFlush handle) (BSC.hPutStrLn handle)

fileAsDestination ::
  Members '[(Embed IO), (Error Text)] m =>
  Sem (Destination ': m) a ->
  Sem ((P.Reader FilePath) ': m) a
fileAsDestination = reinterpret @Destination @(P.Reader FilePath) $ \case
  Publish messages -> do
    path <- P.ask
    handle <- fromExceptionVia @IOException show $ openFile path AppendMode
    embed $ makeWriter handle messages -- should this handle exceptions as well?

stdoutAsDestination ::
  Member (Embed IO) m =>
  Sem (Destination ': m) a ->
  Sem m a
stdoutAsDestination = interpret $ \case
  Publish messages -> embed $ makeWriter stdout messages -- should this handle exceptions as well?

fileAsSource ::
  Members '[(Embed IO), (Error Text)] m =>
  Sem (Source ': m) a ->
  Sem ((P.Reader FilePath) ': m) a
fileAsSource = reinterpret @Source @(P.Reader FilePath) $ \case
  Receive -> do
    path <- P.ask
    handle <- fromExceptionVia @IOException show $ openFile path ReadMode
    embed $ makeReader handle -- should this handle exceptions as well?
  Acknowledge _ -> pure ()

stdinAsSource ::
  Member (Embed IO) m =>
  Sem (Source ': m) a ->
  Sem m a
stdinAsSource = interpret $ \case
  Receive -> embed $ makeReader stdin
  Acknowledge _ -> pure ()
