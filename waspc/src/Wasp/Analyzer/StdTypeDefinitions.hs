{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wasp.Analyzer.StdTypeDefinitions
  ( stdTypes,
  )
where

import Wasp.Analyzer.StdTypeDefinitions.App.Dependency ()
import Wasp.Analyzer.StdTypeDefinitions.Entity ()
import qualified Wasp.Analyzer.TypeDefinitions as TD
import Wasp.Analyzer.TypeDefinitions.TH (makeDeclType, makeEnumType)
import Wasp.AppSpec.Action (Action)
import Wasp.AppSpec.Api (Api, HttpMethod)
import Wasp.AppSpec.ApiNamespace (ApiNamespace)
import Wasp.AppSpec.App (App)
import Wasp.AppSpec.App.Db (DbSystem)
import Wasp.AppSpec.App.EmailSender (EmailProvider)
import Wasp.AppSpec.Crud (Crud)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.Job (Job, JobExecutor)
import Wasp.AppSpec.Page (Page)
import Wasp.AppSpec.Query (Query)
import Wasp.AppSpec.Route (Route)

makeEnumType ''EmailProvider
makeEnumType ''DbSystem
makeDeclType ''Page
makeDeclType ''Route
makeDeclType ''Query
makeDeclType ''Action
makeEnumType ''JobExecutor
makeDeclType ''Job
makeEnumType ''HttpMethod
makeDeclType ''Api
makeDeclType ''ApiNamespace
makeDeclType ''Crud
makeDeclType ''App

{- ORMOLU_DISABLE -}
-- | Collection of domain types that are standard for Wasp, that define what the Wasp language looks like.
-- These are injected this way instead of hardcoding them into the Analyzer in order to make it
-- easier to modify and maintain the Wasp compiler/language.

-- *** MAKE SURE TO UPDATE: The `validateUniqueDeclarationNames` function in the `Wasp.AppSpec.Valid` module
--                          when you add a new declaration type here, we need to check for duplicate declaration names
--                          for the new declaration type.
stdTypes :: TD.TypeDefinitions
stdTypes =
  TD.addDeclType @App $
  TD.addEnumType @DbSystem $
  TD.addDeclType @Entity $
  TD.addDeclType @Page $
  TD.addDeclType @Route $
  TD.addDeclType @Query $
  TD.addDeclType @Action $
  TD.addEnumType @JobExecutor $
  TD.addDeclType @Job $
  TD.addEnumType @HttpMethod $
  TD.addDeclType @Api $
  TD.addDeclType @ApiNamespace $
  TD.addEnumType @EmailProvider $
  TD.addDeclType @Crud $
  TD.empty
{- ORMOLU_ENABLE -}
