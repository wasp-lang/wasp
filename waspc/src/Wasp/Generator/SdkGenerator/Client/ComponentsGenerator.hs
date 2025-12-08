module Wasp.Generator.SdkGenerator.Client.ComponentsGenerator
  ( genClientComponents,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C

genClientComponents :: AppSpec -> Generator [FileDraft]
genClientComponents _spec =
  sequence
    [ genFileCopy [relfile|client/components/DefaultRootErrorBoundary.tsx|],
      genFileCopy [relfile|client/components/FullPageWrapper.tsx|],
      genFileCopy [relfile|client/components/Loader.tsx|],
      genFileCopy [relfile|client/components/Loader.module.css|],
      genFileCopy [relfile|client/components/Message.tsx|]
    ]
  where
    genFileCopy = return . C.mkTmplFd
