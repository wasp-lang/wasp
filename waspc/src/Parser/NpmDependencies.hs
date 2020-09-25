module Parser.NpmDependencies
    ( npmDependencies
    ) where

import qualified Data.Aeson                as Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.HashMap.Strict       as M
import           Text.Parsec               (try)
import           Text.Parsec.String        (Parser)

import qualified Lexer                     as L
import qualified NpmDependency             as ND
import qualified Parser.Common             as P
import           Wasp.NpmDependencies      (NpmDependencies)
import qualified Wasp.NpmDependencies      as NpmDependencies


npmDependencies :: Parser NpmDependencies
npmDependencies = try $ do
    L.reserved L.reservedNameDependencies
    closureContent <- P.waspNamedClosure "json"
    let jsonBytestring = BLU.fromString $ "{ " ++ closureContent ++ " }"
    npmDeps <- case Aeson.eitherDecode' jsonBytestring :: Either String (M.HashMap String String) of
        Left errorMessage -> fail $ "Failed to parse dependencies JSON: " ++ errorMessage
        Right rawDeps     -> return $ map rawDepToNpmDep (M.toList rawDeps)
    return NpmDependencies.NpmDependencies
        { NpmDependencies._dependencies = npmDeps
        }
  where
    rawDepToNpmDep :: (String, String) -> ND.NpmDependency
    rawDepToNpmDep (name, version) = ND.NpmDependency { ND._name = name, ND._version = version }
