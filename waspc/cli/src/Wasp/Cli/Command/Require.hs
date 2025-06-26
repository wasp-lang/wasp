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
    InWaspProject (InWaspProject),
    DbConnectionEstablishedFromOutDir (DbConnectionEstablishedFromOutDir),
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
import Wasp.Generator.DbGenerator.Operations (isDbConnectionPossible, testDbConnection)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Common as Project.Common

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
        let dotWaspRootFilePath = absCurrentDirFp FP.</> SP.fromRelFile Project.Common.dotWaspRootFileInWaspProjectDir
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

data DbConnectionEstablishedFromOutDir = DbConnectionEstablishedFromOutDir deriving (Typeable)

-- TODO(carlos): Modify this requirement to work in `.wasp/build` as well
--
-- Why is this requirement specific to `.wasp/out`? For the time being, most of
-- our DB are designed to work in the context of `wasp start`, and the functions
-- end up depending on two specific outputs:
-- 1. the `.env.server` file, as copied to `<WaspProjectDir>/server/.env`
-- 2. the `schema.prisma` file, processed by the Wasp compiler (to add Auth
--   entities), and copied to `<WaspProjectDir>/db/schema.prisma`
--
-- The `wasp start` command generates these files, but `wasp build` does not, as
-- it expects the environment variables to be given to the resulting Docker
-- container. Moreover, the processing of the `schema.prisma` file is done
-- inside the Docker image building process, so the user's local filesystem
-- doesn't see the final schema file, making it unavailable for the compiler
-- running normally in the user's environment.

instance Requirable DbConnectionEstablishedFromOutDir where
  checkRequirement = do
    -- NOTE: 'InWaspProject' does not depend on this requirement, so this
    -- call to 'require' will not result in an infinite loop.
    InWaspProject waspProjectDir <- require
    let outDir =
          waspProjectDir
            SP.</> Project.Common.dotWaspDirInWaspProjectDir
            SP.</> Project.Common.generatedCodeDirInDotWaspDir
    dbIsRunning <- liftIO $ isDbConnectionPossible <$> testDbConnection outDir

    if dbIsRunning
      then return DbConnectionEstablishedFromOutDir
      else throwError noDbError
    where
      noDbError =
        CommandError
          "Can not connect to database"
          ( "The database needs to be running in order to execute this command."
              ++ " You can easily start a managed dev database with `wasp start db`."
          )
