module ScriptFilter (scriptFilter, defaultFilter, parseAction) where

import qualified Data.ByteString.Char8 as BSC (ByteString, breakSubstring, hGetLine, hPutStrLn, null)
import Filter
import Message
import System.IO (hFlush)
import System.Process

contains :: BSC.ByteString -> BSC.ByteString -> Bool
contains text pattern =
  let (_, after) = BSC.breakSubstring pattern text
   in not $ BSC.null after

actionChecker :: BSC.ByteString -> FilterAction -> BSC.ByteString -> FilterActions
actionChecker pattern action line =
  let actions = if line `contains` pattern then [action] else []
   in MkFilterActions actions

actionCheckers :: [BSC.ByteString -> FilterActions]
actionCheckers = [actionChecker "ack" Ack, actionChecker "copy" Copy]

parseAction :: BSC.ByteString -> FilterActions
parseAction line = mconcat $ actionCheckers <*> [line]

serializeMessage :: Message -> BSC.ByteString
serializeMessage (MkMessage _ _ msg) = msg

scriptFilter :: FilePath -> Message -> IO FilterActions
scriptFilter path msg = do
  (Just inHandle, Just outHandle, _, _) <- createProcess (proc path []) {std_in = CreatePipe, std_out = CreatePipe}
  _ <- BSC.hPutStrLn inHandle (serializeMessage msg)
  hFlush inHandle
  line <- BSC.hGetLine outHandle
  return $ parseAction line

defaultFilter :: Message -> IO FilterActions
defaultFilter _ = return $ MkFilterActions [Copy, Ack]
