module ScriptFilter(scriptFilter, defaultFilter, parseAction) where

import Filter
import Message
import System.Process

import qualified Data.ByteString.Char8 as BSC(null, breakSubstring, hGetLine, hPutStrLn, ByteString)
import System.IO(hFlush)

serializeMessage :: Message -> BSC.ByteString
serializeMessage (MkMessage _ _ msg) = msg

contains :: BSC.ByteString -> BSC.ByteString -> Bool
contains text pattern = let (_, after) = BSC.breakSubstring pattern text
                         in not $ BSC.null after

parseAction :: BSC.ByteString -> FilterAction
parseAction line = if line `contains` "ack" then Ack else Copy
--parseAction line = let containsAck = contains "ack" line
--                       containsCopy = contains "copy" line
--                    in case (containsAck, containsCopy) of
--                         (True, True) -> CopyAndAck
--                         (True, _) -> Ack
--                         (_, True) -> Copy
--                         _ -> error "implement no action case"

scriptFilter :: FilePath -> Message -> IO FilterAction
scriptFilter path msg = do
  (Just inHandle, Just outHandle, _, _) <- createProcess (proc path []){ std_in = CreatePipe, std_out = CreatePipe }
  _ <- BSC.hPutStrLn inHandle (serializeMessage msg)
  hFlush inHandle
  line <- BSC.hGetLine outHandle
  return $ parseAction line

defaultFilter :: Message -> IO FilterAction
defaultFilter _ = return CopyAndAck
