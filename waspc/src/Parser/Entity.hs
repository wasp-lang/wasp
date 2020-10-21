module Parser.Entity
    ( entity
    ) where

import Text.Parsec.String (Parser)
import qualified Data.Text as Text

import qualified Wasp.Entity
import qualified Parser.Common as P
import qualified Lexer as L

entity :: Parser Wasp.Entity.Entity
entity = do
    (name, pslModelSchema) <- P.waspElementNameAndClosure L.reservedNameEntity
                                                          (P.waspNamedClosure "psl")

    return Wasp.Entity.Entity
        { Wasp.Entity._name = name
        , Wasp.Entity._pslModelSchema = Text.pack $ pslModelSchema
        }
