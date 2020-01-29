module Parser.EntityList
    ( entityList
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (spaces)

import qualified Wasp.EntityList as EL
import Wasp.EntityList (EntityList)

import qualified Parser.Common as P
import qualified Lexer as L

-- * EntityList

-- | Parses entity list, e.g. "entity-list<Task> TaskList {...}"
entityList :: Parser EntityList
entityList = do
    (entityName, listName, _) <-
        -- NOTE(matija): not supporting any options yet.
        P.waspElementLinkedToEntity L.reservedNameEntityList spaces

    return EL.EntityList
        { EL._name = listName
        , EL._entityName = entityName
        }
