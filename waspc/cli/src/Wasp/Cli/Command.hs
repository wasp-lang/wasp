{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command
  ( Command,
    runCommand,
    CommandError (..),

    -- * Requirements
    require,
    DbConnectionRequirement (DbConnectionRequirement),
    WaspRootRequirement (WaspRootRequirement),
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT, runExceptT, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Data (Typeable, cast)
import Data.Maybe (fromJust, mapMaybe)
import qualified StrongPath as SP
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory)
import System.Exit (exitFailure)
import qualified System.FilePath as FP
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Generator.DbGenerator.Operations (isDbRunning)
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)

newtype Command a = Command {_runCommand :: StateT [Requirement] (ExceptT CommandError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CommandError)

runCommand :: Command a -> IO ()
runCommand cmd = do
  runExceptT (flip evalStateT [] $ _runCommand cmd) >>= \case
    Left cmdError -> do
      cliSendMessage $ Msg.Failure (_errorTitle cmdError) (_errorMsg cmdError)
      exitFailure
    Right _ -> return ()

-- TODO: What if we want to recognize errors in order to handle them?
--   Should we add _commandErrorType? Should CommandError be parametrized by it, is that even possible?
data CommandError = CommandError {_errorTitle :: !String, _errorMsg :: !String}

data Requirement where
  Requirement :: Requirable r => r -> Requirement

class Typeable r => Requirable r where
  -- | Check if the requirement is met and return a value representing that
  -- requirement.
  --
  -- This function must always return a value: if the requirement is not met,
  -- throw a 'CommandError'.
  checkRequirement :: Command r

-- | Require some dependency or information. See instances of 'Requirable'.
-- Pattern match on the result of the function to determine the requirement
-- and possibly get data from the requirement:
--
-- @
-- do
--   DbConnectionRequirement <- require
-- @
require :: Requirable r => Command r
require =
  Command (gets (mapMaybe cast)) >>= \case
    (req : _) -> return req
    [] -> do
      -- Requirement hasn't been met, so run the check
      req <- checkRequirement
      Command $ modify (Requirement req :)
      return req

data DbConnectionRequirement = DbConnectionRequirement deriving (Typeable)

instance Requirable DbConnectionRequirement where
  checkRequirement = do
    -- NOTE: 'WaspRootRequirement' does not depend on this requirement, so this
    -- call to 'require' will not result in an infinite loop.
    WaspRootRequirement waspRoot <- require
    let outDir = waspRoot SP.</> Cli.Common.dotWaspDirInWaspProjectDir SP.</> Cli.Common.generatedCodeDirInDotWaspDir
    dbIsRunning <- liftIO $ isDbRunning outDir
    if dbIsRunning
      then return DbConnectionRequirement
      else throwError noDbError
    where
      noDbError =
        CommandError
          "Can not connect to database"
          ( "The database needs to be running in order to execute this command."
              ++ " You can easily start a managed dev database with `wasp start db`."
          )

-- | Require a Wasp project to exist near the current directory. Get the
-- project directory by pattern matching on the result of 'require':
--
-- @
-- do
--   WaspRootRequirement waspRoot <- require
-- @
newtype WaspRootRequirement = WaspRootRequirement (SP.Path' SP.Abs (SP.Dir WaspProjectDir)) deriving (Typeable)

instance Requirable WaspRootRequirement where
  checkRequirement = do
    -- Recursively searches up from CWD until @.wasproot@ file is found, or
    -- throw an error if it is never found.
    currentDir <- fromJust . SP.parseAbsDir <$> liftIO getCurrentDirectory
    findWaspProjectRoot currentDir
    where
      findWaspProjectRoot currentDir = do
        let absCurrentDirFp = SP.fromAbsDir currentDir
        doesCurrentDirExist <- liftIO $ doesPathExist absCurrentDirFp
        unless doesCurrentDirExist (throwError notFoundError)
        let dotWaspRootFilePath = absCurrentDirFp FP.</> SP.fromRelFile Cli.Common.dotWaspRootFileInWaspProjectDir
        isCurrentDirRoot <- liftIO $ doesFileExist dotWaspRootFilePath
        if isCurrentDirRoot
          then return $ WaspRootRequirement $ SP.castDir currentDir
          else do
            let parentDir = SP.parent currentDir
            when (parentDir == currentDir) (throwError notFoundError)
            findWaspProjectRoot parentDir

      notFoundError =
        CommandError
          "Wasp command failed"
          ( "Couldn't find wasp project root - make sure"
              ++ " you are running this command from a Wasp project."
          )
