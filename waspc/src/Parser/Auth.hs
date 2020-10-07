module Parser.Auth
  ( auth
  ) where

import Text.Parsec.String (Parser)
import Text.Parsec ((<|>))

import qualified Wasp.Auth
import qualified Parser.Common as P
import qualified Lexer as L

auth :: Parser Wasp.Auth.Auth
auth = do
    L.reserved L.reservedNameAuth
    authProperties <- P.waspClosure (L.commaSep1 authProperty)

    return Wasp.Auth.Auth
        { Wasp.Auth._userEntity = getAuthPropUserEntity authProperties
        , Wasp.Auth._methods = getAuthPropMethods authProperties
        }

-- Auxiliary data structure used by parser.
data AuthProperty = AuthPropertyUserEntity String | AuthPropertyMethods [Wasp.Auth.AuthMethod]

getAuthPropUserEntity :: [AuthProperty] -> String
getAuthPropUserEntity props = head [s | AuthPropertyUserEntity s <- props]

getAuthPropMethods :: [AuthProperty] -> [Wasp.Auth.AuthMethod]
getAuthPropMethods props = head [ms | AuthPropertyMethods ms <- props]

-- Sub-parsers

authProperty :: Parser AuthProperty
authProperty = authPropertyUserEntity <|> authPropertyMethods

authPropertyUserEntity :: Parser AuthProperty
authPropertyUserEntity = AuthPropertyUserEntity <$> (P.waspProperty "userEntity" L.identifier)

authPropertyMethods :: Parser AuthProperty
authPropertyMethods = do
    authMethods <- P.waspProperty "methods" (L.brackets $ L.commaSep1 authMethod)
    return (AuthPropertyMethods authMethods)

authMethod :: Parser Wasp.Auth.AuthMethod
authMethod = L.symbol "EmailAndPassword" *> (pure Wasp.Auth.EmailAndPassword)
