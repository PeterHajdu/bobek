module Main (main) where

import Bobek.Destination
import Bobek.FileEnv
import Bobek.Filter
import Bobek.Log
import qualified Bobek.Message as M
import Bobek.Mover
import Bobek.OptParse
import Bobek.RabbitMqEnv
import Bobek.ScriptFilter
import Bobek.Source
import Data.Semigroup ()
import Data.Text
import qualified Network.AMQP as AMQP (fromURI)
import Polysemy
import Polysemy.Error
import qualified Polysemy.Reader as P

type Interpreter layer =
  forall a m.
  Members '[Embed IO, Error Text] m =>
  Sem (layer ': m) a ->
  Sem m a

destination :: DestinationOpts -> Interpreter Destination
destination = \case
  Stdout -> stdoutAsDestination
  (Outfile (Path filePath)) -> P.runReader (toString filePath) . fileAsDestination
  (Exchange (Uri uri) ex maybeRk) -> stdoutAsDestination -- TODO do rabbit
    --destination (AMQP.fromURI . toString $ uri) ex (unKey <$> maybeRk)

source :: SourceOpts -> Interpreter Source
source = \case
  Stdin -> stdinAsSource
  (Infile (Path filePath)) -> P.runReader (toString filePath) . fileAsSource
  (Queue (Uri uri) queueName) -> stdinAsSource -- TODO do rabbit
    -- createRabbitMqSource (AMQP.fromURI . toString $ uri) queueName

filter' :: FilterOpts -> Interpreter Filter
filter' _ = noopInterpreter -- TODO make normal filter things

-- createFilter :: FilterOpts -> (M.Message -> IO FilterActions)
-- createFilter DontAck = const . pure $ MkFilterActions [Copy]
-- createFilter (FilterScript scriptPath) = scriptFilter $ toString scriptPath

main :: IO ()
main = do
  opts <- runArgParser
  let dstInterpreter = destination $ dst opts
      srcInterpreter = source $ src opts
      logInterpreter = if debug opts then logWithDebug else logOnlyErrors
      filterInterpreter = maybe noopInterpreter filter' (messageFilter opts)

  result <-
    moveMessages
      & dstInterpreter
      & srcInterpreter
      & logInterpreter
      & filterInterpreter
      & runError
      & runM

  case result of
    Left e -> print e
    _ -> pure ()
