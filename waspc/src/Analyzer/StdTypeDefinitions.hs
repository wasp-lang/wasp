{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Analyzer.StdTypeDefinitions
  ( stdTypes,
  )
where

import Analyzer.Evaluator.TH (makeDeclType, makeEnumType)
import qualified Analyzer.TypeDefinitions as TD
import AppSpec.AST.Domain.App (App)
import AppSpec.AST.Domain.AuthMethod (AuthMethod)
import AppSpec.AST.Domain.Page (Page)

makeEnumType ''AuthMethod
makeDeclType ''Page
makeDeclType ''App

{- ORMOLU_DISABLE -}
-- | Collection of domain types that are standard for Wasp, that define what the Wasp language looks like.
-- These are injected this way instead of hardcoding them into the Analyzer in order to make it
-- easier to modify and maintain the Wasp compiler/language.
stdTypes :: TD.TypeDefinitions
stdTypes =
  TD.addEnumType @AuthMethod $
  TD.addDeclType @Page $
  TD.addDeclType @App $
  TD.empty
{- ORMOLU_ENABLE -}
