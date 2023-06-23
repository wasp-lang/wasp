module Wasp.Cli.Command.Require
  ( -- * Asserting Requirements

    -- There are some requirements we want to assert in command code, such as
    -- ensuring the command is being run inside a wasp project directory. We
    -- might end up wanting to check each requirement multiple times, especially
    -- if we want the value from it (like getting the wasp project directory),
    -- but we also want to avoid duplicating work. Using 'require' results in
    -- checked requirements being stored so they can be immediately retrieved
    -- when checking the same requirements additional times.
    --
    -- For example, you can check if you are in a wasp project by doing
    --
    -- @
    -- do
    --   InWaspProject waspProjectDir <- require
    -- @
    --
    -- See instances of 'Requirable' for what kinds of requirements are
    -- supported. To implement a new requirable type, give your type an instance
    -- of 'Requirable'.
    require,

    -- * Requirables
    Requirable (checkRequirement),
    DbConnectionEstablished (DbConnectionEstablished),
    InWaspProject (InWaspProject),
  )
where

import Control.Monad (unless, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory)
import qualified System.FilePath as FP
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Generator.DbGenerator.Operations (isDbRunning)

data DbConnectionEstablished = DbConnectionEstablished deriving (Typeable)

instance Requirable DbConnectionEstablished where
  checkRequirement = do
    -- NOTE: 'InWaspProject' does not depend on this requirement, so this
    -- call to 'require' will not result in an infinite loop.
    InWaspProject waspProjectDir <- require
    let outDir = waspProjectDir SP.</> Cli.Common.dotWaspDirInWaspProjectDir SP.</> Cli.Common.generatedCodeDirInDotWaspDir
    dbIsRunning <- liftIO $ isDbRunning outDir
    if dbIsRunning
      then return DbConnectionEstablished
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
--   InWaspProject waspProjectDir <- require
-- @
newtype InWaspProject = InWaspProject (SP.Path' SP.Abs (SP.Dir WaspProjectDir)) deriving (Typeable)

instance Requirable InWaspProject where
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
          then return $ InWaspProject $ SP.castDir currentDir
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
