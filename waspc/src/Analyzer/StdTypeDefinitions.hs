{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Analyzer.StdTypeDefinitions
  ( stdTypes,
  )
where

import qualified AST
import Analyzer.Evaluator.TH (makeDeclType, makeEnumType)
import qualified Analyzer.TypeDefinitions as TD

makeEnumType ''AST.AuthMethod
makeDeclType ''AST.Page

makeDeclType ''AST.App

{- ORMOLU_DISABLE -}
-- | Collection of domain types that are standard for Wasp, that define what the Wasp language looks like.
-- These are injected this way instead of hardcoding them into the Analyzer in order to make it
-- easier to modify and maintain the Wasp compiler/language.
stdTypes :: TD.TypeDefinitions
stdTypes =
  TD.addEnumType @AST.AuthMethod $
  TD.addDeclType @AST.Page $
  TD.addDeclType @AST.App $
  TD.empty
{- ORMOLU_ENABLE -}
