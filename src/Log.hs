module Log(Logger(..), ioErrorLog, ioDebugLog, LogFunctions(..), logOnlyErrors, logWithDebug) where

import GHC.IO.Handle.FD(stderr)
import Data.Text
import qualified Data.Text.IO as TIO(hPutStrLn)
import qualified Data.Text as T(concat)
import Control.Monad.IO.Class(MonadIO, liftIO)

class Monad m => Logger m where
  logError :: Text -> m ()
  logDebug :: Text -> m ()

data LogFunctions = MkLogFunctions {
  errorFunction :: Text -> IO (),
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
