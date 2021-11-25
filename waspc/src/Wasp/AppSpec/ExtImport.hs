{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
  )
where

import Data.Data (Data)

data ExtImport = ExtImport ExtImportName ExtImportPath
  deriving (Show, Eq, Data)

type ExtImportPath = String

type Identifier = String

data ExtImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    ExtImportModule Identifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    ExtImportField Identifier
  deriving (Show, Eq, Data)
