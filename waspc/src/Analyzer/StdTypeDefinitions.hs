{-# LANGUAGE TypeApplications #-}

module Analyzer.StdTypeDefinitions
  ( AuthMethod (..),
    App (..),
    stdTypes,
  )
where

import Analyzer.Evaluator.TH (makeDeclType, makeEnumType)
import qualified Analyzer.TypeDefinitions as TD

{- ORMOLU_DISABLE -}
-- | Collection of domain types that are standard for Wasp, that define what the Wasp language looks like.
-- These are injected this way instead of hardcoding them into the Analyzer in order to make it
-- easier to modify and maintain the Wasp compiler/language.
stdTypes :: TD.TypeDefinitions
stdTypes =
  TD.addEnumType @AuthMethod $
  TD.addDeclType @App $
  TD.empty
{- ORMOLU_ENABLE -}

-- | TODO: Remove these types from here and instead use types from the Wasp module (Wasp AST).
-- For that we will need to make sure that those types are correctly shaped so that our
-- TH functions can automatically create appropriate instances for them.

--------- MOCK TYPES ----------
data AuthMethod = EmailAndPassword deriving (Show, Eq)

data App = App {title :: String, authMethod :: AuthMethod} deriving (Show, Eq)

makeEnumType ''AuthMethod

makeDeclType ''App

-------- / MOCK TYPES ---------
