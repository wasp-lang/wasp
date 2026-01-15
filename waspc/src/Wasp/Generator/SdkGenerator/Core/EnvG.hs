module Wasp.Generator.SdkGenerator.Core.EnvG
  ( genEnv,
  )
where

import StrongPath (relfile)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genEnv :: Generator [FileDraft]
genEnv =
  return
    [ mkTmplFd [relfile|env/index.ts|],
      mkTmplFd [relfile|env/validation.ts|]
    ]
