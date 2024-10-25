module Wasp.Generator.SdkGenerator.EnvValidation
  ( genEnvValidation,
    depsRequiredByEnvValidation,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C

genEnvValidation :: AppSpec -> Generator [FileDraft]
genEnvValidation _spec =
  sequence
    [ genFileCopy [relfile|env/index.ts|],
      genFileCopy [relfile|client/env/index.ts|],
      genFileCopy [relfile|server/env/index.ts|]
    ]
  where
    genFileCopy = return . C.mkTmplFd

depsRequiredByEnvValidation :: [AS.Dependency.Dependency]
depsRequiredByEnvValidation =
  AS.Dependency.fromList
    [ ("zod", "^3.23.8")
    ]
