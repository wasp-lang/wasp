module Wasp.Job.Fictional where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Env (EnvVar, HasEnvVars (setEnvVars), addEnvVarsOverride)

data Job e a

instance Functor (Job e) where
  fmap = undefined

instance Applicative (Job e) where
  pure = undefined
  liftA2 = undefined

instance Monad (Job e) where
  (>>=) = undefined

instance MonadIO (Job e) where
  liftIO = undefined

run :: (MonadIO m, MonadError e m) => Job e a -> m a
run = undefined

race :: Job e a -> Job e b -> Job e (Either a b)
race = undefined

data OutputKind = Stdout | Stderr

emitJobOutput :: OutputKind -> Text -> Job e ()
emitJobOutput = undefined

captureOutput :: Job e ExitCode -> Job e (ExitCode, Text)
captureOutput = undefined

maybeFailWith :: (ExitCode -> Maybe e) -> Job e ExitCode -> Job e ()
maybeFailWith = undefined

fromProc :: P.CreateProcess -> Job e ExitCode
fromProc = undefined

data JobKind = Wasp | Server | WebApp | Db

prefixWith :: JobKind -> Job e a -> Job e a
prefixWith _kind = undefined

-- This should go in a helper somewhere, not this file
inheritEnv :: (MonadIO m, HasEnvVars a) => a -> m a
inheritEnv x = liftIO $ setEnvVars x <$> getEnvironment

-- This should go in a helper somewhere, not this file
inheritEnvWith :: (MonadIO m, HasEnvVars a) => [EnvVar] -> a -> m a
inheritEnvWith extraEnvVars x =
  liftIO $
    setEnvVars x . (`addEnvVarsOverride` extraEnvVars)
      <$> getEnvironment
