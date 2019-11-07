module ScriptFilter(scriptFilter, defaultFilter) where

import Filter
import Message

scriptFilter :: FilePath -> Message -> IO FilterAction
scriptFilter _ _ = return CopyAndAck

defaultFilter :: Message -> IO FilterAction
defaultFilter _ = return CopyAndAck
