module Wasp.Psl.Parser.Common
  ( SourceCode,
    Parser,
  )
where

import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
  )

type Parser = Parsec Void SourceCode

type SourceCode = String
