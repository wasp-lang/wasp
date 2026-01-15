module Wasp.Generator.SdkGenerator.Core.ClientG
  ( genClient,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genClient :: AppSpec -> Generator [FileDraft]
genClient _spec =
  sequence
    [ return $ mkTmplFd [relfile|client/hooks.ts|]
    ]
