{-# LANGUAGE TypeApplications #-}

module Wasp.Analyzer.StdTypeDefinitions
  ( stdTypes,
  )
where

import Wasp.Analyzer.StdTypeDefinitions.Entity ()
import qualified Wasp.Analyzer.TypeDefinitions as TD
import Wasp.AppSpec.Entity (Entity)

stdTypes :: TD.TypeDefinitions
stdTypes = TD.addDeclType @Entity TD.empty
