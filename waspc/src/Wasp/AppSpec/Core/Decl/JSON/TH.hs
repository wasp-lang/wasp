{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasp.AppSpec.Core.Decl.JSON.TH
  ( generateFromJsonInstanceForDecl,
  )
where

import Control.Monad (forM)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Functor ((<&>))
import Language.Haskell.TH
import Wasp.AppSpec.Core.Decl (Decl, makeDecl)
import Wasp.AppSpec.Core.IsDecl (IsDecl (declTypeName))

generateFromJsonInstanceForDecl :: Q [Dec]
generateFromJsonInstanceForDecl = do
  isDeclTypes <- reifyIsDeclTypes

  caseMatches <- forM isDeclTypes caseMatchForIsDeclType

  -- _ -> fail $ "Unknown declType " <> declType
  defaultCaseMatch <-
    [e|fail $ "Unknown declType " <> declType|]
      <&> \body -> [Match WildP (NormalB body) []]

  [d|
    instance FromJSON Decl where
      parseJSON = withObject "Decl" $ \o -> do
        declType <- o .: "declType"
        declName <- o .: "declName"
        -- case declType of
        --   <caseMatches>
        --   <defultCaseMatch>
        $(pure $ CaseE (VarE (mkName "declType")) (caseMatches <> defaultCaseMatch))
    |]
  where
    -- Generates following (for e.g. `Page` type):
    --   t | t == declTypeName @Page -> pure $ makeDecl @Page declName <$> o .: "declValue"
    caseMatchForIsDeclType :: Type -> Q Match
    caseMatchForIsDeclType typ = do
      guardPredicate <- [|t == $(pure $ AppTypeE (VarE 'declTypeName) typ)|]
      matchBody <- [e|$(pure $ AppTypeE (VarE 'makeDecl) typ) declName <$> (o .: "declValue")|]
      pure $ Match (VarP (mkName "t")) (GuardedB [(NormalG guardPredicate, matchBody)]) []

    reifyIsDeclTypes :: Q [Type]
    reifyIsDeclTypes = do
      ClassI _ isDeclInstances <- reify ''IsDecl
      pure [t | InstanceD _ _ (AppT _ t) _ <- isDeclInstances]
