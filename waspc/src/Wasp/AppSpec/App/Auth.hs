{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethods (..),
    generateIsAuthMethodEnabled,
  )
where

import Data.Data (Data)
import Data.Maybe (isJust)
import Language.Haskell.TH
import Wasp.AppSpec.App.Auth.AuthMethods (AuthMethod, generateAuthMethods)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.Util (toLowerFirst)

$(generateAuthMethods)

data Auth = Auth
  { userEntity :: Ref Entity,
    externalAuthEntity :: Maybe (Ref Entity),
    methods :: AuthMethods,
    onAuthFailedRedirectTo :: String,
    onAuthSucceededRedirectTo :: Maybe String
  }
  deriving (Show, Eq, Data)

generateIsAuthMethodEnabled :: Q [Dec]
generateIsAuthMethodEnabled = do
  let authMethodNames = map (\method -> (mkName . show $ method, (mkName . toLowerFirst . show) method)) [minBound .. maxBound :: AuthMethod]
  clauses <- mapM generateClause authMethodNames
  return [FunD (mkName "isAuthMethodEnabled") clauses]
  where
    generateClause :: (Name, Name) -> Q Clause
    generateClause (authMethodCtor, authMethodName) = do
      let authVar = mkName "auth"
      body <- [|isJust ($(varE authMethodName) ($(varE (mkName "methods")) $(varE authVar)))|]
      return $ Clause [ConP authMethodCtor [], VarP authVar] (NormalB body) []
