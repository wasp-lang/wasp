-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- Todo:
-- The types in the "Wasp" module should have their enum/decl instances created
-- and used here to add to the standard types. This will take some refactoring
-- so that those types follow the restrictions set by "IsDeclType" and
-- "IsEnumType".
--
-- The types currently in this file are just for testing, and should be removed
-- after the above is finished.

module Analyzer.StdTypeDefinitions
  ( AuthMethod (..),
    App (..),
    stdTypes,
  )
where

import Analyzer.Evaluator.TH (makeDeclType, makeEnumType)
import qualified Analyzer.TypeDefinitions as TD

data AuthMethod = EmailAndPassword deriving (Show, Eq)

data App = App {title :: String, authMethod :: AuthMethod} deriving (Show, Eq)

makeEnumType ''AuthMethod

makeDeclType ''App

-- | A Wasp Library containing all of the standard types required for Wasp to
--   work.
stdTypes :: TD.TypeDefinitions
stdTypes =
  {- ORMOLU_DISABLE -}
  TD.addEnumType @AuthMethod $
  TD.addDeclType @App $
  TD.empty
  {- ORMOLU_ENABLE -}
