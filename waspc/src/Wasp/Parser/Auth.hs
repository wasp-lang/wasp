module Wasp.Parser.Auth
  ( auth,
  )
where

import Control.Monad (when)
import Text.Parsec (try, (<|>))
import Text.Parsec.String (Parser)
import qualified Wasp.Lexer as L
import qualified Wasp.Parser.Common as P
import qualified Wasp.Wasp.Auth as Wasp.Auth

auth :: Parser Wasp.Auth.Auth
auth = do
  L.reserved L.reservedNameAuth
  authProperties <- P.waspClosure (L.commaSep1 authProperty)

  let userEntityProps = [s | AuthPropertyUserEntity s <- authProperties]
  failIfPropMissing propUserEntityName userEntityProps

  let methodsProps = [ms | AuthPropertyMethods ms <- authProperties]
  failIfPropMissing propMethodsName methodsProps

  let failRedirectProps = [r | AuthPropertyOnAuthFailedRedirectTo r <- authProperties]
  failIfPropMissing propOnAuthFailedRedirectToName failRedirectProps

  let successRedirectProps = [r | AuthPropertyOnAuthSucceededRedirectTo r <- authProperties]

  return
    Wasp.Auth.Auth
      { Wasp.Auth._userEntity = head userEntityProps,
        Wasp.Auth._methods = head methodsProps,
        Wasp.Auth._onAuthFailedRedirectTo = head failRedirectProps,
        Wasp.Auth._onAuthSucceededRedirectTo = headWithDefault "/" successRedirectProps
      }

headWithDefault :: a -> [a] -> a
headWithDefault d ps = if null ps then d else head ps

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
  | AuthPropertyOnAuthSucceededRedirectTo String

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
    <|> (try authPropertyOnAuthFailedRedirectTo <|> authPropertyOnAuthSucceededRedirectTo)

authPropertyOnAuthFailedRedirectTo :: Parser AuthProperty
authPropertyOnAuthFailedRedirectTo =
  AuthPropertyOnAuthFailedRedirectTo <$> P.waspPropertyStringLiteral "onAuthFailedRedirectTo"

authPropertyOnAuthSucceededRedirectTo :: Parser AuthProperty
authPropertyOnAuthSucceededRedirectTo =
  AuthPropertyOnAuthSucceededRedirectTo <$> P.waspPropertyStringLiteral "onAuthSucceededRedirectTo"

authPropertyUserEntity :: Parser AuthProperty
authPropertyUserEntity = AuthPropertyUserEntity <$> P.waspProperty "userEntity" L.identifier

authPropertyMethods :: Parser AuthProperty
authPropertyMethods = AuthPropertyMethods <$> P.waspProperty "methods" (L.brackets $ L.commaSep1 authMethod)

authMethod :: Parser Wasp.Auth.AuthMethod
authMethod = L.symbol "EmailAndPassword" *> pure Wasp.Auth.EmailAndPassword
