module Log(Logger(..), ioErrorLog, ioDebugLog) where

import Data.Text
import qualified Data.Text.IO as TIO(putStrLn)
import qualified Data.Text as T(concat)
import Control.Monad.IO.Class(MonadIO, liftIO)

class Monad m => Logger m where
  logError :: Text -> m ()
  logDebug :: Text -> m ()

ioErrorLog :: MonadIO m => Text -> m ()
ioErrorLog = ioLog "ERROR "

ioDebugLog :: MonadIO m => Text -> m ()
ioDebugLog = ioLog "DEBUG "

ioLog :: MonadIO m => Text -> Text -> m ()
ioLog prefix msg = liftIO $ TIO.putStrLn $ T.concat [prefix, msg]
