{-# LANGUAGE TemplateHaskell #-}

module Bobek.Log
  ( Logger (..),
    logError,
    logDebug,
    logOnlyErrors,
    logWithDebug,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO (hPutStrLn)
import Polysemy
import Polysemy.Error

data Logger m a where
  LogError :: Text -> Logger m ()
  LogDebug :: Text -> Logger m ()

makeSem ''Logger

ioLog :: Text -> Text -> IO ()
ioLog prefix msg = TIO.hPutStrLn stderr $ T.concat [prefix, msg]

logOnlyErrors ::
  Members '[(Embed IO)] m =>
  Sem (Logger ': m) a ->
  Sem m a
logOnlyErrors = interpret $ \case
  LogError msg -> embed $ ioLog "ERROR" msg
  LogDebug msg -> pure ()

logWithDebug ::
  Members '[(Embed IO)] m =>
  Sem (Logger ': m) a ->
  Sem m a
logWithDebug = interpret $ \case
  LogError msg -> embed $ ioLog "ERROR" msg
  LogDebug msg -> embed $ ioLog "DEBUG" msg
