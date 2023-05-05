module Wasp.Generator.ServerGenerator.CrudG
  ( genCrud,
  )
where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)

genCrud :: AppSpec -> Generator [FileDraft]
genCrud spec = return []
