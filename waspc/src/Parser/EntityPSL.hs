module Parser.EntityPSL
    ( entityPSL
    ) where

import Text.Parsec.String (Parser)
import qualified Data.Text as Text

import qualified Wasp.EntityPSL
import qualified Parser.Common as P
import qualified Lexer as L

entityPSL :: Parser Wasp.EntityPSL.EntityPSL
entityPSL = do
    (name, pslModelSchema) <- P.waspElementNameAndClosure L.reservedNameEntityPSL
                                                          (P.waspNamedClosure "psl")

    return Wasp.EntityPSL.EntityPSL
        { Wasp.EntityPSL._name = name
        , Wasp.EntityPSL._pslModelSchema = Text.pack $ pslModelSchema
        }
