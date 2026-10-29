module Wasp.Psl.Format
  ( prismaFormat,
    PrismaFormatResult (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, Path', fromAbsDir)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import qualified System.Process as P
import Wasp.Generator.DbGenerator.Jobs (absPrismaExecutableFp)
import Wasp.Project.Common (WaspProjectDir)

-- | For given prisma schema source, returns formatted schema + any warnings/errors,
-- by running `prisma format` of the project's own Prisma on it.
-- "prisma format" does more than just formatting -> it also applies some obvious fixes,
-- like missing relationship fields. So it is kind of like compiling + formatting + fixing.
-- It works even for a prisma schema that has only model declarations!
--
-- The project's dependencies must be installed, since Prisma comes from the
-- project's node_modules.
prismaFormat :: Path' Abs (Dir WaspProjectDir) -> Text -> IO PrismaFormatResult
prismaFormat waspProjectDir prismaSchema =
  withSystemTempFile "schema.prisma" $ \schemaFilePath schemaFileHandle -> do
    hClose schemaFileHandle
    T.IO.writeFile schemaFilePath prismaSchema
    let formatProcess =
          (P.proc (absPrismaExecutableFp waspProjectDir) ["format", "--schema", schemaFilePath])
            { P.cwd = Just $ fromAbsDir waspProjectDir
            }
    (exitCode, _stdout, stderr) <- P.readCreateProcessWithExitCode formatProcess ""
    formattedSchemaPsl <- T.IO.readFile schemaFilePath
    return
      PrismaFormatResult
        { _formattedSchemaPsl = formattedSchemaPsl,
          _schemaErrors = case exitCode of
            ExitSuccess -> Nothing
            ExitFailure _ -> Just $ T.pack stderr
        }

data PrismaFormatResult = PrismaFormatResult
  { _formattedSchemaPsl :: Text,
    _schemaErrors :: Maybe Text
  }
  deriving (Show)
