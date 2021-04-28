module Parser.Auth
  ( auth,
  )
where

import Control.Monad (when)
import qualified Lexer as L
import qualified Parser.Common as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Wasp.Auth

auth :: Parser Wasp.Auth.Auth
auth = do
  L.reserved L.reservedNameAuth
  authProperties <- P.waspClosure (L.commaSep1 authProperty)

  let userEntityProps = [s | AuthPropertyUserEntity s <- authProperties]
  failIfPropMissing propUserEntityName userEntityProps

  let methodsProps = [ms | AuthPropertyMethods ms <- authProperties]
  failIfPropMissing propMethodsName methodsProps

  let redirectProps = [r | AuthPropertyOnAuthFailedRedirectTo r <- authProperties]
  failIfPropMissing propOnAuthFailedRedirectToName redirectProps

  return
    Wasp.Auth.Auth
      { Wasp.Auth._userEntity = head userEntityProps,
        Wasp.Auth._methods = head methodsProps,
        Wasp.Auth._onAuthFailedRedirectTo = head redirectProps
      }

-- TODO(matija): this should be extracted if we want to use in other places too.
failIfPropMissing :: (Applicative m, MonadFail m) => String -> [p] -> m ()
failIfPropMissing propName ps = when (null ps) $ fail errorMsg
  where
    errorMsg = propName ++ " is required!"

-- Auxiliary data structure used by parser.
data AuthProperty
  = AuthPropertyUserEntity String
  | AuthPropertyMethods [Wasp.Auth.AuthMethod]
  | AuthPropertyOnAuthFailedRedirectTo String

propUserEntityName :: String
propUserEntityName = "userEntity"

propMethodsName :: String
propMethodsName = "methods"

propOnAuthFailedRedirectToName :: String
propOnAuthFailedRedirectToName = "onAuthFailedRedirectTo"

-- Sub-parsers

authProperty :: Parser AuthProperty
authProperty =
  authPropertyUserEntity
    <|> authPropertyMethods
    <|> authPropertyOnAuthFailedRedirectTo

authPropertyOnAuthFailedRedirectTo :: Parser AuthProperty
authPropertyOnAuthFailedRedirectTo =
  AuthPropertyOnAuthFailedRedirectTo <$> (P.waspPropertyStringLiteral "onAuthFailedRedirectTo")

authPropertyUserEntity :: Parser AuthProperty
authPropertyUserEntity = AuthPropertyUserEntity <$> (P.waspProperty "userEntity" L.identifier)

authPropertyMethods :: Parser AuthProperty
authPropertyMethods = AuthPropertyMethods <$> P.waspProperty "methods" (L.brackets $ L.commaSep1 authMethod)

authMethod :: Parser Wasp.Auth.AuthMethod
authMethod = L.symbol "EmailAndPassword" *> (pure Wasp.Auth.EmailAndPassword)
