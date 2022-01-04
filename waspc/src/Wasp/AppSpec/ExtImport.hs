{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
  )
where

import Data.Data (Data)
import StrongPath (File', Path, Posix, Rel)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)

data ExtImport = ExtImport
  { -- | What is being imported.
    name :: ExtImportName,
    -- | Path from which we are importing.
    -- TODO: Before, in Wasp, this was a StrongPath relative file! But now it is just string.
    --   Why is that so? We should probably again make it to be StrongPath.
    --   Actually when I think about it now, this probably means we are not even checking right now
    --   if this is valid path! So we should do that checking by parsing it into StrongPath
    --   and then storing StrongPath here. I am not sure if we should store Posix Rel File or just
    --   System Rel File, but we can figure that out.
    --   This validation / parsing we can probably do by implement an instance of HasCustomEvaluation,
    --   I am not sure though if we should implement it for StrongPath in general or for ExtImport specifically here,
    --   probably we should try for StrongPath and then if that doesn't work, specifically for ExtImport.
    path :: ExtImportPath
  }
  deriving (Show, Eq, Data)

type ExtImportPath = Path Posix (Rel SourceExternalCodeDir) File'

type Identifier = String

data ExtImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    ExtImportModule Identifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    ExtImportField Identifier
  deriving (Show, Eq, Data)
