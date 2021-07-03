module Generator.WebAppGenerator.OperationsGenerator.ResourcesG
  ( genResources,
  )
where

import Data.Aeson (object)
import Generator.FileDraft (FileDraft)
import qualified Generator.WebAppGenerator.Common as C
import StrongPath (relfile)
import Wasp (Wasp)

genResources :: Wasp -> [FileDraft]
genResources _ = [C.makeTemplateFD tmplFile dstFile (Just tmplData)]
  where
    tmplFile = C.asTmplFile [relfile|src/operations/resources.js|]
    dstFile = C.asWebAppFile [relfile|src/operations/resources.js|] -- TODO: Un-hardcode this by combining path to operations dir with path to resources file in it.
    tmplData = object []
