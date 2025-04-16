module Wasp.Node.Internal
  ( parseVersionFromCommandOutput,
  )
where

import qualified Text.Parsec as P
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.SemanticVersion.Version as SV

type ErrorMessage = String

parseVersionFromCommandOutput :: String -> Either ErrorMessage SV.Version
parseVersionFromCommandOutput commandOutput =
  case findAndParseVersion commandOutput of
    Left parseError -> Left $ makeFailedToParseVersionErrorMessage parseError
    Right version -> Right version
  where
    findAndParseVersion = P.parse versionParser ""
    versionParser = skipAnyCharTillMatch SV.versionParser
    skipAnyCharTillMatch p = P.manyTill P.anyChar (P.lookAhead $ P.try p) >> p

    makeFailedToParseVersionErrorMessage parseError =
      unlines
        [ "Wasp failed to parse version from string '" ++ commandOutput ++ "'.",
          show parseError,
          "This is most likely a bug in Wasp, please file an issue at https://github.com/wasp-lang/wasp/issues."
        ]
