module Wasp.Generator.WebAppGenerator.OperationsGenerator.ResourcesG
  ( genResources,
  )
where

import Data.Aeson (object)
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.WebAppGenerator.Common as C

genResources :: AppSpec -> Generator [FileDraft]
genResources _ = return [C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)]
  where
    tmplFile = C.asTmplFile [relfile|src/operations/resources.js|]
    dstFile = C.asWebAppFile [relfile|src/operations/resources.js|] -- TODO: Un-hardcode this by combining path to operations dir with path to resources file in it.
    tmplData = object []
