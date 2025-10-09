{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Wasp.AppSpec.Core.Decl.JSON.TH
  ( generateFromJsonInstanceForDecl,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Functor ((<&>))
import Language.Haskell.TH
import Wasp.AppSpec.Core.Decl (Decl, makeDecl)
import Wasp.AppSpec.Core.IsDecl (IsDecl (declTypeName))

generateFromJsonInstanceForDecl :: Q [Dec]
generateFromJsonInstanceForDecl = do
  declTypes <- reifyInstancesOfIsDeclClass
  caseMatches <- mapM getCaseMatchForDeclType declTypes

  -- Generates:
  --   _ -> fail $ "Unknown declType " <> declType
  defaultCaseMatch <-
    [e|fail $ "Unknown declType " <> declType|]
      <&> \body -> [Match WildP (NormalB body) []]

  [d|
    instance FromJSON Decl where
      parseJSON = withObject "Decl" $ \o -> do
        declType <- o .: "declType"
        declName <- o .: "declName"
        -- Generates:
        --   case declType of
        --     <caseMatches[0]>
        --     <caseMatches[1]>
        --     ...
        --     <defultCaseMatch>
        $( pure $
             CaseE
               (VarE (mkName "declType"))
               (caseMatches <> defaultCaseMatch)
         )
    |]
  where
    -- Generates following (for e.g. `Page` type):
    --   t | t == declTypeName @Page -> makeDecl @Page declName <$> o .: "declValue"
    getCaseMatchForDeclType :: Type -> Q Match
    getCaseMatchForDeclType typ = do
      casePredicate <- [|t == $(pure $ AppTypeE (VarE 'declTypeName) typ)|]
      matchBody <-
        [e|
          $(pure $ AppTypeE (VarE 'makeDecl) typ)
            declName
            <$> (o .: "declValue")
          |]
      pure $
        Match
          (VarP (mkName "t"))
          (GuardedB [(NormalG casePredicate, matchBody)])
          []

    reifyInstancesOfIsDeclClass :: Q [Type]
    reifyInstancesOfIsDeclClass = do
      ClassI _ isDeclInstances <- reify ''IsDecl
      pure [t | InstanceD _ _ (AppT _ t) _ <- isDeclInstances]
