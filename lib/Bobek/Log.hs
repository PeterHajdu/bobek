module Bobek.Log (Logger (..), ioErrorLog, ioDebugLog, LogFunctions (..), logOnlyErrors, logWithDebug) where

import qualified Data.Text as T (concat)
import qualified Data.Text.IO as TIO (hPutStrLn)

class Monad m => Logger m where
  logError :: Text -> m ()
  logDebug :: Text -> m ()

data LogFunctions = MkLogFunctions
  { errorFunction :: Text -> IO (),
    debugFunction :: Text -> IO ()
  }

logOnlyErrors :: LogFunctions
logOnlyErrors = MkLogFunctions ioErrorLog (const $ return ())

logWithDebug :: LogFunctions
logWithDebug = MkLogFunctions ioErrorLog ioDebugLog

ioErrorLog :: MonadIO m => Text -> m ()
ioErrorLog = ioLog "ERROR "

ioDebugLog :: MonadIO m => Text -> m ()
ioDebugLog = ioLog "DEBUG "

ioLog :: MonadIO m => Text -> Text -> m ()
ioLog prefix msg = liftIO $ TIO.hPutStrLn stderr $ T.concat [prefix, msg]
