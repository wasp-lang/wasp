-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- Todo:
-- The types in the "Wasp" module should have their enum/decl instances created
-- and used here to add to the standard types. This will take some refactoring
-- of those types so that the instances of the right field names.

module Analyzer.StdTypeDefinitions
  ( AuthMethod (..),
    App (..),
    stdTypes,
  )
where

import Analyzer.Evaluator.TH (makeDecl, makeEnum)
import qualified Analyzer.TypeDefinitions as TD

data AuthMethod = EmailAndPassword deriving (Show, Eq)

data App = App {title :: String, authMethod :: AuthMethod} deriving (Show, Eq)

makeEnum ''AuthMethod

makeDecl ''App

-- | A Wasp Library containing all of the standard types required for Wasp to
--   work.
stdTypes :: TD.TypeDefinitions
stdTypes =
  {- ORMOLU_DISABLE -}
  TD.addEnumType @AuthMethod $
  TD.addDeclType @App $
  TD.empty
  {- ORMOLU_ENABLE -}
