{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions
  ( stdTypes,
  )
where

import qualified Wasp.Analyzer.TypeDefinitions as TD
import Wasp.Analyzer.TypeDefinitions.TH (makeDeclType, makeEnumType)
import Wasp.AppSpec.Action (Action)
import Wasp.AppSpec.App (App)
import Wasp.AppSpec.App.Auth (AuthMethod)
import Wasp.AppSpec.App.Db (DbSystem)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.Page (Page)
import Wasp.AppSpec.Query (Query)
import Wasp.AppSpec.Route (Route)

makeEnumType ''AuthMethod
makeEnumType ''DbSystem
makeDeclType ''Entity
makeDeclType ''App
makeDeclType ''Page
makeDeclType ''Route
makeDeclType ''Query
makeDeclType ''Action

{- ORMOLU_DISABLE -}
-- | Collection of domain types that are standard for Wasp, that define what the Wasp language looks like.
-- These are injected this way instead of hardcoding them into the Analyzer in order to make it
-- easier to modify and maintain the Wasp compiler/language.
stdTypes :: TD.TypeDefinitions
stdTypes =
  TD.addDeclType @App $
  TD.addEnumType @AuthMethod $
  TD.addEnumType @DbSystem $
  TD.addDeclType @Entity $
  TD.addDeclType @Page $
  TD.addDeclType @Route $
  TD.addDeclType @Query $
  TD.addDeclType @Action $
  TD.empty
{- ORMOLU_ENABLE -}
