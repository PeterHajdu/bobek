module Log(Logger(..)) where

import Data.Text

class Monad m => Logger m where
  logError :: Text -> m ()
  logDebug :: Text -> m ()

