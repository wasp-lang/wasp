module Wasp.Psl.Format
  ( prismaFormat,
    PrismaFormatResult,
    PslModelText,
    PslErrorsMsg,
    prismaFormatModels,
  )
where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import qualified System.Process as P
import qualified Wasp.NodePackageFFI as WP
import Wasp.Util.Aeson (decodeFromString)

-- | For given prisma schema source, returns formatted schema + any warnings/errors,
-- by calling "prisma format" in the background.
-- It might fail while attempting to do all that, in which case it will return error message.
-- "prisma format" does more than just formatting -> it also applies some obvious fixes,
-- like missing relationship fields. So it is kind of like compiling + formatting + fixing.
-- It works even for a prisma schema that has only model declarations!
prismaFormat :: Text -> IO PrismaFormatResult
prismaFormat prismaSchema = do
  cp <- WP.getPackageProcessOptions WP.PrismaPackage ["format"]
  (exitCode, response, stderr) <- P.readCreateProcessWithExitCode cp $ T.unpack prismaSchema
  case exitCode of
    ExitSuccess ->
      return $
        decodeFromString response
          & either (error . ("Failed to parse response json from wasp's prisma ts package: " <>)) id
    _exitFailure -> error $ "Failed while calling prisma format via wasp's prisma ts package: " <> stderr

data PrismaFormatResult = PrismaFormatResult
  { _formattedSchemaPsl :: Text,
    _schemaErrors :: Maybe Text
  }
  deriving (Show)

instance Aeson.FromJSON PrismaFormatResult where
  parseJSON = Aeson.withObject "PrismaFormatResult" $ \obj -> do
    formattedSchemaPsl <- obj .: "formattedSchemaPsl"
    errors <- obj .:? "errors"
    return (PrismaFormatResult {_formattedSchemaPsl = formattedSchemaPsl, _schemaErrors = errors})

type PslModelText = Text

type PslErrorsMsg = Text

-- | Given a list of psl models in textual format (e.g. ["model User {\n...\n}", ...]),
-- it returns back a list of those models but formatted, and also prisma format errors message,
-- if there are any errors.
prismaFormatModels :: [PslModelText] -> IO (Maybe PslErrorsMsg, [PslModelText])
prismaFormatModels models = do
  let schema = T.intercalate ("\n" <> delimiter <> "\n") models
  result <- prismaFormat schema
  let formattedModels = T.strip <$> T.splitOn delimiter (_formattedSchemaPsl result)
  return (_schemaErrors result, formattedModels)
  where
    delimiter = "//==== WASP ====//"
